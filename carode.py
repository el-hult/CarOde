import numpy as np
import scipy.integrate
import pandas as pd
import altair as alt

clamp1 = lambda x: min(1, max(x, -1))

tmax = 20.0
tmax = 4.5e-11

g = 9.81  # [m/s^2] gravitational acceleration
wheel_radius = 0.34  # [m]
inertia = 5  # [kg m] moment of inertia of wheel (+drive line)
mass = 1500  # [kg] mass of the care


def sigma(omega, v):
    """Compute the slip ratio"""
    if v != 0:
        return (omega * wheel_radius - v) / abs(v)
    elif omega == 0:
        return 0
    else:
        return np.sign(omega) * np.inf


def t_drive(t):
    """Torque from the engine onto the wheel, in Nm"""
    if t < 3:
        return 200
    elif t < 4:
        return 0
    elif t < 7:
        return -500
    elif t < 8:
        return 5500
    elif t < 13:
        return 2000
    else:
        return 0


def f_drag(v):
    C = 2  # drag constant dependant on geometry, surface etc
    return -C * v * np.abs(v)


def f_roll(v):
    C = 0.7  # rolling resistance constant depends on stuffs
    return -C * v


def func(t, y, t_drive):
    v, omega = y
    slip_ratio = sigma(omega, v)
    f_traction = clamp1(slip_ratio / 0.06) * mass * g
    a = (f_traction + f_drag(v) + f_roll(v)) / mass
    alpha = (-f_traction * wheel_radius + t_drive(t)) / inertia
    return [a, alpha]


# LSODA can switch between stiff and nonstiff solver
# to get precision AND speed
method = "LSODA"
y0 = np.array([0, 0])
args = (t_drive,)
res = scipy.integrate.solve_ivp(
    fun=func, y0=y0, t_span=[0, tmax], args=args, method=method
)
y = res.y
t = res.t
print(res.message)
print(f"Solving took {res.nfev} function evaluations to complete")
v = y[0, :]
omega = y[1, :]
[as_,alphas] = list(zip(*[func(t,y,t_drive) for t,y in zip(t,y.T)]))


#
# Plot the results!
#
C_t = "Time (s)"
C_v = "Car speed (m/s)"
C_omegaR = "Wheel speed (m/s)"
C_drive = "Drive torque (kNm)"
C_roll = "Roll resistance (kN)"
C_drag = "Drag force (kN)"
C_sr = "Slip ratio (%)"
C_acc = "Acceleration (m/s^2)"
C_wAcc = "Wheel Acceleration (m/s^2)"
data = pd.DataFrame(
    {
        C_t: t,
        C_v: v,
        C_omegaR: omega * wheel_radius,
        C_drive: [t_drive(s) / 1000 for s in t],
        C_drag: [f_drag(w) / 1000 for w in v],
        C_roll: [f_roll(w) / 1000 for w in v],
        C_sr: [100 * sigma(om, ve) for om, ve in zip(omega, v)],
        C_acc: as_,
        C_wAcc: [alpha*wheel_radius for alpha in alphas],
    }
)
alt.Chart(data).transform_fold(
    fold=[C_v, C_omegaR, C_drive, C_sr, C_drag, C_roll, C_acc, C_wAcc], as_=["Data", "Value"]
).mark_line(point=alt.OverlayMarkDef()).encode(
    alt.Y("Value", type="quantitative", scale=alt.Scale(domain=(-2, 10))),
    alt.X(C_t),
    alt.Color("Data", type="nominal"),
).properties(
    width=600, height=600
).interactive().show()
