-- Engine constants, NOT settings (do not change)
local LAND_HEIGHT = 2.0          -- Maximum height above ground at which you can "land"
local NON_JUMP_VELOCITY = 140.0  -- Maximum Z velocity you are allowed to have and still land
local MIN_STANDABLE_ZNRM = 0.7   -- Minimum surface normal Z component of a walkable surface
local AIR_SPEED_CAP = 30.0       -- Constant used to limit air acceleration
local DUCK_MIN_DUCKSPEED = 1.5   -- Minimum duckspeed to start ducking
local DEFAULT_JUMP_IMPULSE = 301.99337741 -- sqrt(2 * 57.0 units * 800.0 u/s^2)

-- Global vars
local PLAYER_HULL_MIN = Vector(-16.0, -16.0, 0.0)
local PLAYER_HULL_STAND = Vector(16.0, 16.0, 62.0)
local PLAYER_HULL_DUCK = Vector(16.0, 16.0, 45.0)
local PLAYER_HULL_DELTA = (PLAYER_HULL_STAND.z - PLAYER_HULL_DUCK.z) / 2

local g_iTick = {}
local g_flFrameTime = {}

local g_iButtons = {}
local g_iOldButtons = {}
local g_vVel = {}
local g_vAngles = {}

local g_iLastTickPredicted = {}
local g_vPreCollisionVelocity = {}
local g_vLastBaseVelocity = {}
local g_bLastOnGround = {}
local g_iLastCollisionTick = {}

local UPHILL_LOSS = -1    -- Force a jump, AND negatively affect speed as if a collision occurred (fix RNG not in player's favor)
local UPHILL_DEFAULT = 0  -- Do nothing (retain RNG)
local UPHILL_NEUTRAL = 1  -- Force a jump (respecting NON_JUMP_VELOCITY) (fix RNG in player's favor)

-- Plugin settings
local g_cvDownhill = CreateConVar("rngfix_downhill", "1", FCVAR_NOTIFY, "Enable downhill incline fix.", 0.0, 1.0)
local g_cvUphill = CreateConVar("rngfix_uphill", "1", FCVAR_NOTIFY, "Enable uphill incline fix. Set to -1 to normalize effects not in the player's favor (not recommended).", -1.0, 1.0)
local g_cvEdge = CreateConVar("rngfix_edge", "1", FCVAR_NOTIFY, "Enable edgebug fix.", 0.0, 1.0)
local g_cvDebug = CreateConVar("rngfix_debug", "0", FCVAR_NONE, "1 = Enable debug messages. 2 = Enable debug messages and lasers.", 0.0, 2.0)
local g_cvUseOldSlopefixLogic = CreateConVar("rngfix_useoldslopefixlogic", "0", FCVAR_NOTIFY, "Old Slopefix had some logic errors that could cause double boosts. Enable this on a per-map basis to retain old behavior. (NOT RECOMMENDED)", 0.0, 1.0)

-- Core physics ConVars
local g_cvMaxVelocity = GetConVar("sv_maxvelocity")
local g_cvGravity = GetConVar("sv_gravity")
local g_cvAirAccelerate = GetConVar("sv_airaccelerate")

-- AutoBhop predicate
local IsAutoHopEnabled = function (ply) return true end 

-- [ Flow bhop compat ] --

hook.Add("Initialize", "RNGFIX_FlowCompat", function()
    if not _C then return end

    DEFAULT_JUMP_IMPULSE = _C.Player.JumpPower
    
    PLAYER_HULL_MIN = _C.Player.HullMin
    PLAYER_HULL_STAND = _C.Player.HullStand
    PLAYER_HULL_DUCK = _C.Player.HullDuck
    PLAYER_HULL_DELTA = (PLAYER_HULL_STAND.z - PLAYER_HULL_DUCK.z) / 2

    local s1, s2 = _C.Style["Easy Scroll"], _C.Style.Legit
    IsAutoHopEnabled = function(ply)
        return ply.Style ~= s1 and ply.Style ~= s2
    end
end)

-- [ Debug ] --

util.AddNetworkString("RNGFIX_DebugLaser")
local g_color1 = Color(0, 100, 255, 255)
local g_color2 = Color(0, 255, 0, 255)

local DebugMsg, DebugLaser
do 
    local function p_DebugMsg(ply, fmt, ...)
        if not g_cvDebug:GetBool() then return end

        local str = string.format(fmt, ...)
        print("[RNGFIX] " .. ply:UserID() .. " " .. g_iTick[ply] .. "\t " .. str)
        ply:PrintMessage(HUD_PRINTCONSOLE, "[RNGFIX] " .. str)
    end

    local function p_DebugLaser(ply, p1, p2, life, width, col)
        if g_cvDebug:GetInt() < 2 then return end

        net.Start("RNGFIX_DebugLaser")
            net.WriteVector(p1)
            net.WriteVector(p2)
            net.WriteFloat(life)
            net.WriteFloat(width)
            net.WriteColor(col)
        net.Send(ply)
    end

    local function nothing() end
    local function toggleDebug(_, _, val)
        if val == "1" then
            DebugMsg, DebugLaser = p_DebugMsg, nothing
        elseif val == "2" then
            DebugMsg, DebugLaser = p_DebugMsg, p_DebugLaser
        else
            DebugMsg, DebugLaser = nothing, nothing
        end
    end
    cvars.AddChangeCallback("rngfix_debug", toggleDebug)
    toggleDebug(_, _, g_cvDebug:GetString())
end

-- [ Player hooks ] --

hook.Add("PlayerInitialSpawn", "RNGFIX", function (ply)
    g_iTick[ply] = 0
    g_flFrameTime[ply] = 0.0

    g_iButtons[ply] = 0
    g_vVel[ply] = Vector()
    g_vAngles[ply] = Angle()
    g_iOldButtons[ply] = 0

    g_iLastTickPredicted[ply] = 0

    g_vPreCollisionVelocity[ply] = Vector()
    g_vLastBaseVelocity[ply] = Vector()
    g_bLastOnGround[ply] = true
    g_iLastCollisionTick[ply] = 0
end)

hook.Add("PlayerDisconnected", "RNGFIX", function (ply)
    g_iTick[ply] = nil
    g_flFrameTime[ply] = nil

    g_iButtons[ply] = nil
    g_vVel[ply] = nil
    g_vAngles[ply] = nil
    g_iOldButtons[ply] = nil

    g_iLastTickPredicted[ply] = nil

    g_vPreCollisionVelocity[ply] = nil
    g_vLastBaseVelocity[ply] = nil
    g_bLastOnGround[ply] = nil
    g_iLastCollisionTick[ply] = nil
end)

-- [ Simulations ] --

local band, traceHull, tickInterval, Vector = bit.band, util.TraceHull, engine.TickInterval, Vector

local function IsDuckCoolingDown(ply)
    -- m_flDuckSpeed is decreased by 2.0 to a minimum of 0.0 every time the duck key is pressed OR released.
    -- It recovers at a rate of 3.0 * m_flLaggedMovementValue per second and caps at 8.0.
    -- Switching to a ducked state from an unducked state is prevented if it is less than 1.5.
    return ply:GetDuckSpeed() < DUCK_MIN_DUCKSPEED
end

local function Duck(ply, origin)
    local ducking = ply:IsFlagSet(FL_DUCKING)
    local nextDucking = ducking

    if band(g_iButtons[ply], IN_DUCK) ~= 0 and not ducking then
        if not IsDuckCoolingDown(ply) then
            origin.z = origin.z + PLAYER_HULL_DELTA
            nextDucking = true
        end
    elseif band(g_iButtons[ply], IN_DUCK) == 0 and ducking then
        local tr = traceHull({
            start = origin,
            endpos = origin,
            mins = PLAYER_HULL_MIN,
            maxs = PLAYER_HULL_STAND,
            mask = MASK_PLAYERSOLID_BRUSHONLY
        })

        -- Can unduck in air, enough room
        if not tr.Hit then
            origin.z = origin.z - PLAYER_HULL_DELTA
            nextDucking = false
        end
    end

    return PLAYER_HULL_MIN, (nextDucking and PLAYER_HULL_DUCK or PLAYER_HULL_STAND), origin
end

local function CanJump(ply)
    if band(g_iButtons[ply], IN_JUMP) == 0 then return false end
    if band(g_iOldButtons[ply], IN_JUMP) ~= 0 and not IsAutoHopEnabled(ply) then return false end

    return true
end

local function CheckVelocity(velocity)
    local max = g_cvMaxVelocity:GetFloat()
    for i = 1, 3 do 
        if velocity[i] > max then
            velocity[i] = max
        elseif velocity[i] < -max then
            velocity[i] = -max
        end
    end
    return velocity
end

local function StartGravity(ply, velocity)
    local localGravity = ply:GetGravity()
    if localGravity == 0.0 then localGravity = 1.0 end

    local baseVelocity = ply:GetBaseVelocity()
    velocity.z = velocity.z + (baseVelocity.z - localGravity * g_cvGravity:GetFloat() * 0.5) * g_flFrameTime[ply]

    -- baseVelocity.z would get cleared here but we shouldn't do that since this is just a prediction.
    return CheckVelocity(velocity)
end

local function FinishGravity(ply, velocity)
    local localGravity = ply:GetGravity()
    if localGravity == 0.0 then localGravity = 1.0 end

    velocity.z = velocity.z - localGravity * g_cvGravity:GetFloat() * 0.5 * g_flFrameTime[ply]

    return CheckVelocity(velocity)
end

local function CheckJumpButton(ply, velocity)
    -- Skip dead and water checks since we already did them.
    -- We need to check for ground somewhere so stick it here.
    if not ply:IsFlagSet(FL_ONGROUND) then return end
    if not CanJump(ply) then return end

    -- This conditional is why jumping while crouched jumps higher! Bad!
    if ply:IsFlagSet(FL_DUCKING) or ply:GetInternalVariable("m_bDucking") ~= 0 then
        velocity.z = DEFAULT_JUMP_IMPULSE
    else
        velocity.z = velocity.z + DEFAULT_JUMP_IMPULSE
    end

    -- Jumping does an extra half tick of gravity! Bad!
    return FinishGravity(ply, velocity)
end

local function AirAccelerate(ply, velocity, mv)
    -- This also includes the initial parts of AirMove()
    local fore = g_vAngles[ply]:Forward()
    local side = g_vAngles[ply]:Right()
    fore.z = 0.0
    side.z = 0.0
    fore:Normalize()
    side:Normalize()

    local wishvel = Vector()
    for i = 1, 2 do wishvel[i] = fore[i] * g_vVel[ply].x + side[i] * g_vVel[ply].y end

    local wishspeed = wishvel:Length()
    local wishdir = wishvel:GetNormalized()

    local m_flMaxSpeed = mv:GetMaxSpeed()
    if wishspeed > m_flMaxSpeed and m_flMaxSpeed ~= 0.0 then
        wishspeed = m_flMaxSpeed
    end

    if wishspeed ~= 0 then
        local wishspd = wishspeed
        if wishspd > AIR_SPEED_CAP then wishspd = AIR_SPEED_CAP end

        local addspeed = wishspd - velocity:Dot(wishdir)

        if addspeed > 0 then
            local accelspeed = g_cvAirAccelerate:GetFloat() * wishspeed * g_flFrameTime[ply]
            if (accelspeed > addspeed) then accelspeed = addspeed end

            for i = 1, 2 do velocity[i] = velocity[i] + accelspeed * wishdir[i] end
        end
    end

    return velocity
end

-- [ Utils ]

local function CheckWater(ply)
    -- The cached water level is updated multiple times per tick, including after movement happens,
    -- so we can just check the cached value here.
    return ply:WaterLevel() > 1
end

local function PreventCollision(ply, mv, origin, collisionPoint, velocity_tick)
    DebugLaser(ply, origin, collisionPoint, 15.0, 0.5, g_color1)

    -- Rewind part of a tick so at the end of this tick    we will end up close to the ground without colliding with it.
    -- This effectively simulates a mid-tick jump (we lose part of a tick but its a miniscule trade-off).
    -- This is also only an approximation of a partial tick rewind but it's good enough.
    local newOrigin = collisionPoint - velocity_tick

    -- Add a little space between us and the ground so we don't accidentally hit it anyway, maybe due to floating point error or something.
    -- I don't know if this is necessary but I would rather be safe.
    newOrigin.z = newOrigin.z + 0.1

    -- Since the MoveData for this tick has already been filled and is about to be used, we need
    -- to modify it directly instead of changing the player entity's actual position (such as with TeleportEntity).
    mv:SetOrigin(newOrigin)

    DebugLaser(ply, origin, newOrigin, 15.0, 0.5, g_color2)
    local adjustment = newOrigin - origin
    DebugMsg(ply, "Moved: %.2f %.2f %.2f", adjustment.x, adjustment.y, adjustment.z)

    -- No longer colliding this tick, clear our prediction flag
    g_iLastCollisionTick[ply] = 0
end

local function ClipVelocity(velocity, nrm)
    local backoff = velocity:Dot(nrm)
    
    -- The adjust step only matters with overbounce which doesnt apply to walkable surfaces.
    return velocity - nrm * backoff
end

local function SetVelocity(ply, velocity, mv)
    if not velocity then return end
    -- Pull out basevelocity from desired true velocity
    -- Use the pre-tick basevelocity because that is what influenced this tick's movement and the desired new velocity.
    velocity:Sub(g_vLastBaseVelocity[ply])
    
    DebugMsg(ply, "SetVelocity: %.2f %.2f %.2f", velocity.x, velocity.y, velocity.z)

    if mv then
        mv:SetVelocity(velocity)
    else
        ply:SetAbsVelocity(velocity)
        ply:SetVelocity(velocity + ply:GetBaseVelocity() - ply:GetVelocity())
        -- ply:SetVelocity(velocity - ply:GetVelocity())
    end
end

local function TracePlayerBBoxForGround(origin, originBelow, mins, maxs, ply)
    -- See CGameMovement::TracePlayerBBoxForGround()
    local origMins, origMaxs = Vector(mins), Vector(maxs)
    local tr = nil

    -- -x -y
    mins = origMins
    maxs = Vector(math.min(origMaxs.x, 0.0), math.min(origMaxs.y, 0.0), origMaxs.z)
    tr = traceHull({
        start = origin,
        endpos = originBelow,
        mins = mins,
        maxs = maxs,
        mask = MASK_PLAYERSOLID_BRUSHONLY,
    })
    if tr.Hit and tr.HitNormal.z >= MIN_STANDABLE_ZNRM then
        return tr
    end

    -- +x +y
    mins = Vector(math.max(origMins.x, 0.0), math.max(origMins.y, 0.0), origMins.z)
    maxs = origMaxs
    tr = traceHull({
        start = origin,
        endpos = originBelow,
        mins = mins,
        maxs = maxs,
        mask = MASK_PLAYERSOLID_BRUSHONLY
    })
    if tr.Hit and tr.HitNormal.z >= MIN_STANDABLE_ZNRM then
        return tr
    end

    -- -x +y
    mins = Vector(origMins.x, math.max(origMins.y, 0.0), origMins.z)
    maxs = Vector(math.min(origMaxs.x, 0.0), origMaxs.y, origMaxs.z)
    tr = traceHull({
        start = origin,
        endpos = originBelow,
        mins = mins,
        maxs = maxs,
        mask = MASK_PLAYERSOLID_BRUSHONLY
    })
    if tr.Hit and tr.HitNormal.z >= MIN_STANDABLE_ZNRM then
        return tr
    end

    -- +x -y
    mins = Vector(math.max(origMins.x, 0.0), origMins.y, origMins.z)
    maxs = Vector(origMaxs.x, math.min(origMaxs.y, 0.0), origMaxs.z)
    tr = traceHull({
        start = origin,
        endpos = originBelow,
        mins = mins,
        maxs = maxs,
        mask = MASK_PLAYERSOLID_BRUSHONLY
    })
    if tr.Hit and tr.HitNormal.z >= MIN_STANDABLE_ZNRM then
        return tr
    end

    return nil
end

-- [ Pre-Tick fixes ]

local function RunPreTickChecks(ply, mv, cmd)
    -- Recreate enough of CGameMovement::ProcessMovement to predict if fixes are needed.
    -- We only really care about a limited set of scenarios (less than waist-deep in water, MOVETYPE_WALK, air movement).

    if not ply:Alive() then return end
    if ply:GetMoveType() ~= MOVETYPE_WALK then return end
    if CheckWater(ply) then return end
    
    -- If we are definitely staying on the ground this tick, don't predict it.
    if ply:IsOnGround() and not CanJump(ply) then 
        return 
    end

    g_iLastTickPredicted[ply] = g_iTick[ply]
    g_iButtons[ply] = mv:GetButtons()
    g_iOldButtons[ply] = mv:GetOldButtons()
    g_vVel[ply] = Vector(mv:GetForwardSpeed(), mv:GetSideSpeed(), mv:GetUpSpeed())
    g_vAngles[ply] = mv:GetAngles()

    local velocity = mv:GetVelocity()
    local baseVelocity = ply:GetBaseVelocity()
    local origin = mv:GetOrigin()

    -- These roughly replicate the behavior of their equivalent CGameMovement functions.
    local mins, maxs, nextOrigin = Duck(ply, Vector(origin))
    StartGravity(ply, velocity)
    CheckJumpButton(ply, velocity)
    CheckVelocity(velocity)
    AirAccelerate(ply, velocity, mv)

    -- StartGravity dealt with Z basevelocity.
    baseVelocity.z = 0.0
    g_vLastBaseVelocity[ply] = baseVelocity
    velocity:Add(baseVelocity)

    -- Store this for later in case we need to undo the effects of a collision.
    g_vPreCollisionVelocity[ply] = velocity

    -- This is basically where TryPlayerMove happens.
    -- We don't really care about anything after TryPlayerMove either.
    
    local velocity_tick = velocity * g_flFrameTime[ply]
    nextOrigin:Add(velocity_tick)
    
    -- Check if we will hit something this tick.
    local tr = traceHull({
        start = origin,
        endpos = nextOrigin,
        mins = mins,
        maxs = maxs,
        mask = MASK_PLAYERSOLID_BRUSHONLY
    })

    if tr.Hit then
        local nrm = tr.HitNormal

        if g_iLastCollisionTick[ply] < g_iTick[ply] - 1 then
            DebugMsg(ply, "Collision predicted! (normal: %.3f %.3f %.3f)", nrm.x, nrm.y, nrm.z)
        end
        
        local collisionPoint = tr.HitPos

        -- Store this result for post-tick fixes.
        g_iLastCollisionTick[ply] = g_iTick[ply]

        -- If we are moving up too fast, we can't land anyway so these fixes aren't needed.
        if velocity.z > NON_JUMP_VELOCITY then return end

        -- Landing also requires a walkable surface.
        -- This will give false negatives if the surface initially collided
        -- is too steep but the final one isn't (rare and unlikely to matter).
        if nrm.z < MIN_STANDABLE_ZNRM then return end

        -- Check uphill incline fix first since it's more common and faster.
        if g_cvUphill:GetInt() == UPHILL_NEUTRAL then
            -- Make sure it's not flat, and that we are actually going uphill (X/Y dot product < 0.0)
            if nrm.z < 1.0 and nrm.x*velocity.x + nrm.y*velocity.y < 0.0 then
                local shouldDoDownhillFixInstead = false

                if g_cvDownhill:GetBool() then
                    -- We also want to make sure this isn't a case where it's actually more beneficial to do the downhill fix.
                    local newVelocity = ClipVelocity(velocity, nrm)

                    if newVelocity.x*newVelocity.x + newVelocity.y*newVelocity.y > velocity.x*velocity.x + velocity.y*velocity.y then
                        shouldDoDownhillFixInstead = true
                        DebugMsg(ply, "shouldDoDownhillFixInstead")
                    end
                end

                if not shouldDoDownhillFixInstead then
                    DebugMsg(ply, "DO FIX: Uphill Incline")
                    PreventCollision(ply, mv, origin, collisionPoint, velocity_tick)

                    -- This naturally prevents any edge bugs so we can skip the edge fix.
                    return
                end
            end
        end

        if g_cvEdge:GetBool() then
            -- Do a rough estimate of where we will be at the end of the tick after colliding.
            -- This method assumes no more collisions will take place after the first.
            -- There are some very extreme circumstances where this will give false positives (unlikely to come into play).

            local fraction_left = 1.0 - tr.Fraction
            local tickEnd

            if nrm.z == 1.0 then
                -- If the ground is level, all that changes is Z velocity becomes zero.
                tickEnd = Vector(
                    collisionPoint.x + velocity_tick.x * fraction_left,
                    collisionPoint.y + velocity_tick.y * fraction_left,
                    collisionPoint.z
                )
            else
                local velocity2 = ClipVelocity(velocity, nrm)
                
                if (velocity2.z > NON_JUMP_VELOCITY) then
                    -- This would be an "edge bug" (slide without landing at the end of the tick)
                    -- 100% of the time due to the Z velocity restriction.
                    return
                else
                    velocity2:Mul(g_flFrameTime[ply] * fraction_left)
                    tickEnd = collisionPoint + velocity2
                end
            end

            -- Check if there's something close enough to land on below the player at the end of this tick.
            local tickEndBelow = Vector(tickEnd.x, tickEnd.y, tickEnd.z - LAND_HEIGHT)
            local tr2 = traceHull({
                start = tickEnd,
                endpos = tickEndBelow,
                mins = mins,
                maxs = maxs,
                mask = MASK_PLAYERSOLID_BRUSHONLY
            })

            if tr2.Hit then
                -- There's something there, can we land on it?
                local nrm2 = tr2.HitNormal

                -- Yes, it's not too steep.
                if nrm2.z >= MIN_STANDABLE_ZNRM then return end
                
                -- Yes, the quadrant check finds ground that isn't too steep.
                if TracePlayerBBoxForGround(tickEnd, tickEndBelow, mins, maxs, ply) then return end
            end

            DebugMsg(ply, "DO FIX: Edge Bug")
            DebugLaser(ply, collisionPoint, tickEnd, 15.0, 0.5, g_color1)

            PreventCollision(ply, mv, origin, collisionPoint, velocity_tick)
        end
    end
end

-- ProcessMovementPre
hook.Add("SetupMove", "RNGFIX", function(ply, mv, cmd)
    g_iTick[ply] = cmd:TickCount()
    g_flFrameTime[ply] = tickInterval() * ply:GetLaggedMovementValue()
    
    -- If we are actually not doing ANY of the fixes that rely on pre-tick collision prediction, skip all this.
    if (g_cvDownhill:GetBool() or g_cvUphill:GetBool() or g_cvEdge:GetBool()) then
        RunPreTickChecks(ply, mv, cmd) 
    end
end)

-- [ Post-Tick fixes ] --

local function DoInclineCollisionFixes(ply, nrm, mv)
    if not g_cvDownhill:GetBool() and g_cvUphill:GetInt() ~= UPHILL_LOSS then return false end
    
    if g_iLastTickPredicted[ply] ~= g_iTick[ply] then return false end

    -- There's no point in checking for fix if we were moving up, unless we want to do an uphill collision
    if g_vPreCollisionVelocity[ply].z > 0.0 and g_cvUphill:GetInt() ~= UPHILL_LOSS then return false end

    -- If a collision was predicted this tick (and wasn't prevented by another fix alrady), no fix is needed.
    -- It's possible we actually have to run the edge bug fix and an incline fix in the same tick.
    -- If using the old Slopefix logic, do the fix regardless of necessity just like Slopefix
    -- so we can be sure to trigger a double boost if applicable.
    if g_iLastCollisionTick[ply] == g_iTick[ply] and not g_cvUseOldSlopefixLogic:GetBool() then return false end

    -- Make sure the ground is not level, otherwise a collision would do nothing important anyway.
    if nrm.z == 1.0 then return false end

    -- This velocity includes changes from player input this tick as well as
    -- the half tick of gravity applied before collision would occur.
    local velocity = g_vPreCollisionVelocity[ply]

    if g_cvUseOldSlopefixLogic:GetBool() then
        -- The old slopefix did not consider basevelocity when calculating deflected velocity
        velocity:Sub(g_vLastBaseVelocity[ply])
    end

    local dot = nrm.x*velocity.x + nrm.y*velocity.y

    if dot >= 0 then
        -- If going downhill, only adjust velocity if the downhill incline fix is on.
        if not g_cvDownhill:GetBool() then return false end
    end

    local downhillFixIsBeneficial = false

    local newVelocity = ClipVelocity(velocity, nrm)

    if newVelocity.x*newVelocity.x + newVelocity.y*newVelocity.y > velocity.x*velocity.x + velocity.y*velocity.y then
        downhillFixIsBeneficial = true
    end

    if dot < 0 then
        -- If going uphill, only adjust velocity if uphill incline fix is set to loss mode
        -- OR if this is actually a case where the downhill incline fix is better.
        if not ((downhillFixIsBeneficial and g_cvDownhill:GetBool()) or g_cvUphill:GetInt() == UPHILL_LOSS) then return false end
    end

    DebugMsg(ply, "DO FIX: Incline Collision (%s) (z-normal: %.3f)", downhillFixIsBeneficial and "Downhill" or "Uphill", nrm.z)

    -- Make sure Z velocity is zero since we are on the ground.
    newVelocity.z = 0.0

    -- Since we are on the ground, we also don't need to FinishGravity().
    if g_cvUseOldSlopefixLogic:GetBool() then
        -- The old slopefix immediately moves basevelocity into local velocity to keep it from getting cleared.
        -- This results in double boosts as the player is likely still being influenced by the source of the basevelocity.
        if ply:IsFlagSet(FL_BASEVELOCITY) then
            newVelocity:Add(ply:GetBaseVelocity())
        end

        if mv then
            mv:SetVelocity(newVelocity)
        else
            ply:SetVelocity(newVelocity - ply:GetVelocity())
        end
    else
        SetVelocity(ply, newVelocity, mv)
    end

    return true
end

TeleFix = {

	UpdateMessage = "TeleHopFix and TriggerFix has been updated 2022",

	Hooks = { -- Associate a function to a hook
		SetupData = "PlayerSpawn",
		CheckOnGround = "SetupMove",
		Move = "Move",
		SurfFix = "FinishMove",
		PredictVelocityClip = "Move",
		CheckNoClip = "PlayerNoClip",
		AnalyseBoosters = "EntityKeyValue",
		PrepareBoosters = "InitPostEntity",
		DisableBoosters = "AcceptInput",
		PrepareTPs = "InitPostEntity",
		PlayerTeleported = "AcceptInput",
	},

	TickRate = 1 / engine.TickInterval(), -- Cache the tickrate
	Boosters = TeleFix and TeleFix.Boosters or {}, -- Cache the boosters

	Initialize = function(self) -- Associate functions to hook and create a function to know if a player is allowed to use TeleFix
		local Hooks = self.Hooks
		setmetatable(self, {__index = _G})
		for name, func in pairs(TeleFix) do
			if not isfunction(func) then continue end
			setfenv(func, self)
			if Hooks[name] then
				hook.Add(Hooks[name], "TeleFix"..name, func)
			end
		end
		
		local Player = FindMetaTable("Player")
		
		Player.UseTeleFix = function(ply, functionality)
			return ply.TeleFix.Enabled
				and (not ply.Spectating)
				and (not ply.TeleFix.NoClip)
				and not ply:IsBot()
		end

		for _, ply in pairs( player.GetAll() ) do
			self.SetupData(ply)
		end

		timer.Simple(0, function() -- Can be used to notify players about an update
--			Core:Broadcast( "Print", { "Notification", self.UpdateMessage } )
		end)
		concommand.Add("update_TeleFix", function(ply)
			if not ply:IsValid() then
				include("autorun/server/sv_telefix.lua")
			end
		end)
	end,

	PrepareTPs = function() -- Hook outputs to trigger_teleport so we can know when a player has been teleported
		for _, ent in pairs( ents.FindByClass("trigger_teleport") ) do
			ent:Fire("AddOutput", "OnStartTouch !activator:teleported:0:0:-1")
			ent:Fire("AddOutput", "OnEndTouch !activator:teleported:0:0:-1")
		end
	end,

	SetupData = function(ply) -- Setup data each time a player spawn
		local enabled
		if ply.TeleFix then enabled = ply.TeleFix.Enabled end
		ply.TeleFix = {
			Enabled = (enabled == nil) and true or enabled,
			PreviousVL = {Vector()},
			PredictedVL = false,
			PreviousPos = Vector(),
			WasOnGround = true,
			Landing = false,
			Landed = false,
			Jumped = false,
			GroundTrace = {},
			InBooster = false,
			OnRamp = false,
			WasOnRamp = false,
			LastOnRamp = 0,
			NoClip = false,
			Teleported = false,
			NextGravity = false,
			NextVelocity = false,
		}
	end,

	CheckNoClip = function(ply, noclip) -- Just to know if a player enabled the noclip
  	  if ply.Practice then
  	      ply.TeleFix.NoClip = noclip
  	  end
	end,

	CheckOnGround = function(ply, mv, cmd) -- Try to know when a player is about to collide with the ground, check if he has landed and store a TraceHull result for later use
		local pFix = ply.TeleFix
		local vl = mv:GetVelocity()
		pFix.Landed = ply:IsOnGround() and not pFix.WasOnGround
		pFix.Jumped = pFix.WasOnGround and not ply:IsOnGround()
		pFix.WasOnGround = ply:IsOnGround()

		if pFix.Landed then
			local endpos = mv:GetOrigin()
			endpos.z = endpos.z - 100
			pFix.GroundTrace = util.TraceHull {
				start = mv:GetOrigin(),
				endpos = endpos,
				mins = ply:OBBMins(),
				maxs = ply:OBBMaxs(),
				mask = MASK_PLAYERSOLID_BRUSHONLY,
			}
			pFix.Landing = false
		elseif pFix.Landing then
			pFix.Landed = true
			pFix.Landing = false
		elseif vl.z < 0 then
			local endpos = mv:GetOrigin()
			endpos = endpos + (FrameTime() * vl)
			local ground = util.TraceHull {
				start = mv:GetOrigin(),
				endpos = endpos,
				mins = ply:OBBMins(),
				maxs = ply:OBBMaxs(),
				mask = MASK_PLAYERSOLID_BRUSHONLY,
			}
			if ground.Hit and (ground.HitNormal.z >= 0.7) then
				pFix.Landing = true
				pFix.GroundTrace = ground
			end
		end
	end,

	PredictVelocityClip = function(ply, mv, cmd) 
		if not ply:UseTeleFix() then return end
		local pFix = ply.TeleFix
		if not pFix.OnRamp then return end

		-- Air acceleration
		local aa, gain = 500, 32.8
		local aim = mv:GetMoveAngles()
		local forward, right = aim:Forward(), aim:Right()
		local fmove = mv:GetForwardSpeed()
		local smove = mv:GetSideSpeed()
		
		forward.z, right.z = 0,0
		forward:Normalize()
		right:Normalize()
	
		local wishvel = forward * fmove + right * smove
		wishvel.z = 0
	
		local wishspeed = wishvel:Length()
		if wishspeed > mv:GetMaxSpeed() then
			wishvel = wishvel * (mv:GetMaxSpeed() / wishspeed)
			wishspeed = mv:GetMaxSpeed()
		end
	
		local wishspd = wishspeed
		wishspd = math.Clamp( wishspd, 0, gain )
	
		local wishdir = wishvel:GetNormal()
		local current = mv:GetVelocity():Dot( wishdir )
	
		local addspeed = wishspd - current
		if addspeed <= 0 then return end
	
		local accelspeed = aa * FrameTime() * wishspeed
		if accelspeed > addspeed then
			accelspeed = addspeed
		end
		
		local vel = mv:GetVelocity()
		local speed = vel:Length2D()
		vel = vel + (wishdir * accelspeed)
		--

		local hitNormal = pFix.GroundTrace.HitNormal
		vel = Vector(vel.x, vel.y, vel.z)
		vel.z = vel.z - (ply:GetGravity() * 800 * FrameTime() * 0.5)

		local slowDown = vel:Dot(hitNormal)
		vel.x = vel.x - hitNormal.x * slowDown
		vel.y = vel.y - hitNormal.y * slowDown
		vel.z = vel.z - hitNormal.z * slowDown

		local adjust = vel:Dot(hitNormal)
		if adjust < 0 then
			vel = vel - (hitNormal * adjust)
		end

		pFix.PredictedVL = vel
	end,

	PlayerTeleported = function(ply, input) -- Called when a player has been teleported
		if ply:IsValid() and ply:IsPlayer() and input == "teleported" then
			ply.TeleFix.Teleported = true
		end
	end,

	Move = function(ply, mv) -- Whole TeleFix Move function to apply fixes in the correct order
		if not ply:UseTeleFix() then return end
		local pFix = ply.TeleFix

		TelehopFix(ply, mv)
		TelehopFix2(ply, mv)

		pFix.WasOnGround = ply:IsOnGround()
		pFix.PreviousPos = mv:GetOrigin()

		local pVL = pFix.PreviousVL
		local max = math.min(#pVL, 9)
		for i = 1, max do
			pVL[i + 1] = pVL[i]
		end
		pVL[1] = mv:GetVelocity()
	end,

	TelehopFix = function(ply, mv) -- Go back 100ms before and apply a velocity rollback when a player has been teleported and loosed speed
		if not ply:UseTeleFix() then return end
		if ply:IsOnGround() then return end

		if game.GetMap() == "bhop_horseshit_4" then return end
		if game.GetMap() == "bhop_overline" then return end
		if game.GetMap() == "bhop_autobadges" then return end
		if game.GetMap() == "bhop_pandora2_fix" then return end
		if game.GetMap() == "bhop_asko_fix" then return end
		if game.GetMap() == "bhop_kotodama_fix" then return end
		if game.GetMap() == "bhop_sonder" then return end

		local pFix = ply.TeleFix

		if not pFix.Teleported then return end

		local pVL = pFix.PreviousVL[10]
		local cVL = mv:GetVelocity()
		if pVL and ( pVL:Length2DSqr() > cVL:Length2DSqr() ) then
			mv:SetVelocity(pVL)
		end
		pFix.Teleported = false
	end,

	TelehopFix2 = function(ply, mv) -- Go back 100ms before and apply a velocity rollback when a player has been teleported and loosed speed
		if not ply:UseTeleFix() then return end
		if not game.GetMap() == "bhop_asko_fix" then return end
		if not game.GetMap() == "bhop_kotodama_fix" then return end
		if not game.GetMap() == "bhop_sonder" then return end

		if game.GetMap() == "bhop_morency" then return end
		if game.GetMap() == "bhop_hathor" then return end
		if game.GetMap() == "bhop_akmal" then return end
		if game.GetMap() == "bhop_vertex_fast" then return end
		if game.GetMap() == "bhop_fizzy" then return end
		if game.GetMap() == "bhop_vale" then return end
		if game.GetMap() == "bhop_blackrockshooter" then return end
		if game.GetMap() == "surf_claz_TeleFix_test" then return end
		if game.GetMap() == "bhop_olipoid" then return end
		if game.GetMap() == "bhop_peepoo" then return end
		if game.GetMap() == "bhop_boeuf_bourguignon" then return end
		if game.GetMap() == "bhop_areaportal_v1" then return end
		if game.GetMap() == "bhop_badges_fix" then return end
		if game.GetMap() == "bhop_badges_mini" then return end
		if game.GetMap() == "bhop_tiramisu2" then return end
		if game.GetMap() == "bhop_growfonder" then return end
		if game.GetMap() == "bhop_malabar5" then return end
		if game.GetMap() == "bhop_ametisti2" then return end
		if game.GetMap() == "bhop_niceday_v2" then return end
		if game.GetMap() == "bhop_eazy_mom" then return end
		if game.GetMap() == "bhop_jayro" then return end

		local pFix = ply.TeleFix

		if not pFix.Teleported then return end

		local pVL = pFix.PreviousVL[10]
		local cVL = mv:GetVelocity()
		if pVL and ( pVL:Length2DSqr() > cVL:Length2DSqr() ) then
			mv:SetVelocity(pVL)
		end
		pFix.Teleported = false
	end,

}

TeleFix:Initialize()

-- PostThink works a little better than a ProcessMovement post hook because we need to wait for ProcessImpacts (trigger activation)
-- PlayerPostThink or FinishMove or Move ?
hook.Add("PlayerPostThink", "RNGFIX", function (ply, mv)
    if not ply:Alive() then return end
    if ply:GetMoveType() ~= MOVETYPE_WALK then return end
    if CheckWater(ply) then return end

    local landed = ply:IsOnGround() and not g_bLastOnGround[ply]
    local landingMins, landingMaxs, nrm, landingPoint = Vector(), Vector(), Vector(), Vector()
    local frac = 0

    g_bLastOnGround[ply] = ply:IsOnGround()

    -- Get info about the ground we landed on (if we need to do landing fixes).
    if landed and (g_cvDownhill:GetBool() or g_cvUphill:GetInt() == UPHILL_LOSS) then
        local origin = ply:GetPos()
        landingMins, landingMaxs = ply:GetCollisionBounds()

        local originBelow = origin - Vector(0, 0, LAND_HEIGHT)

        local tr = traceHull({
            start = origin,
            endpos = originBelow,
            mins = landingMins,
            maxs = landingMaxs,
            mask = MASK_PLAYERSOLID_BRUSHONLY
        })

        if not tr.Hit then
            -- This should never happen, since we know we are on the ground.
            landed = false
        else
            nrm = tr.HitNormal
            frac = tr.Fraction

            if nrm.z < MIN_STANDABLE_ZNRM then
                -- This is rare, and how the incline fix should behave isn't entirely clear because maybe we should
                -- collide with multiple faces at once in this case, but let's just get the ground we officially
                -- landed on and use that for our ground normal.

                -- landingMins and landingMaxs will contain the final values used to find the ground after returning.
                local tr2 = TracePlayerBBoxForGround(origin, originBelow, landingMins, landingMaxs, ply)
                if tr2 then
                    nrm = tr2.HitNormal
                else
                    -- This should also never happen.
                    landed = false
                end

                DebugMsg(ply, "Used bounding box quadrant to find ground (z-normal: %.3f)", nrm[2])
            end

            landingPoint = tr.HitPos
        end
    end

    if landed then
        DoInclineCollisionFixes(ply, nrm, mv)
    end
end)
