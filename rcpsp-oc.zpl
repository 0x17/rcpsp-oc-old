# MIP model for RCPSP-Q with possibility to book overtimes at constant price.
# This simplified version only considers 1 type of renewable resource and no
# non-renewables.

# TODO: Move params to external file

param timeHorizon := 10;
set T := { 1 .. timeHorizon };

param nRes := 1;
set R := { 1 .. nRes };
param K[R] := <1> 2;

param kappa[R] := <1> 1; # cost per unit of overtime
param zMax[R] := <1> 2; # maximum overtime capacity

param nJobs := 5;
param last := nJobs+1;
set J := { 0 .. nJobs+1 };
set actual := J without { 0, nJobs+1 };

param d[J] := <0> 0, <1> 1, <2> 1, <3> 2, <4> 2, <5> 1, <6> 0;
param k[R*J] :=	  | 0, 1, 2, 3, 4, 5, 6 |
                |1| 0, 1, 2, 1, 1, 2, 0 |;
set edges := {<0, 1>, <1, 2>, <2, 3>, <0, 4>, <4, 5>, <5, 6>, <3, 6>};

set L := { "A", "AA", "AAA" }; # quality levels
set Lstar := {"A", "AA"}; # reached levels

# revenue for project finishing in level l and w/ makespan t
param q[L*T] :=       |  4,  5,  6,  7,  8,  9, 10 |
                |"AAA"| 50, 40, 30, 20, 10,  9,  8 |
                | "AA"| 40, 30, 20, 10,  9,  8,  7 |
                |  "A"| 30, 20, 10,  9,  8,  7,  6 | default 0;

defset periodsActive(j, t) := { min(timeHorizon, t) .. min(t+d[j]-1, timeHorizon)};

var x[J*T] binary;
var z[R*T] integer >= 0;
var makespan integer >= 0;
#var revenue integer >= 0;

minimize costs: makespan
				+ (sum <r> in R : sum <t> in T : z[r,t] * kappa[r]); # total overcapacity costs

#subto syncrevenue: revenue == (max <l> in Lstar : q[l, makespan]);
subto syncmakespan: makespan == sum <t> in T : t * x[last, t];

subto eachjobonce:
	forall <j> in J do
		(sum <t> in T : x[j,t]) == 1;

subto precedence:
	forall <i,j> in edges do
		(sum <t> in T : t*x[i,t]) <= (sum <t> in T : (t-d[j])*x[j,t]);

subto rescapacity:
	forall <r> in R do
		forall <t> in T do
			(sum <j> in actual : k[r, j] * sum <tau> in periodsActive(j, t) : x[j, tau]) <= K[r] + z[r, t];

subto overtimecapacity:
	forall <r> in R do
		forall <t> in T do
			z[r,t] <= zMax[r];
