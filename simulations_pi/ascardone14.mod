/*
 * This file replicates the model studied in:
 * Ascari, Guido and Sbordone, Argia M. (2014): "The Macroeconomics of Trend Inflation",
 * Journal of Economic Literature, 52(3), pp. 679-739.
 * 
 * It provides a replication of the main results of the original paper  
 * in Section 3 (the New Keynesian model with trend inflation). It replicates the Figures:
 * - Figure 7: The Cost of Price dispersion
 * - Figure 8: Trend Inflation and Steady State Variables
 * - Figure 11: The Determinacy Region and Trend Inflation
 * - Figure 13: Impulse Response Functions to a 1 Percent Positive Technology Shock 
 * - Figure 14: Impulse Response Functions to a 1 Percent Positive Monetary Policy Shock 
 *
 * Moreover, it replicates the business cycle moments reported on p. 717. 
 *
 * This mod-file shows how to access steady state variables in order to plot steady state
 * dependences on parameters.
 * It also shows how to manually do a stability mapping be iterating over a grid on the parameter space.
 * 
 * Notes:
 * - The mod-file requires Dynare > 4.4.3, i.e. currently the unstable version. Otherwise, you will get error like 
 *      "ERROR: in the 'steady_state' block, variable 'pi' is undefined in the declaration of variable 'pi'"
 * - The results from the nonlinear model have been cross-checked with the linearized 
 *      version presented in the paper and the replication files provided by the authors
 * - The annual inflation target is disaggregated to quarterly figures using the geometric mean. Simply dividing by
 *      4 results in small numerical differences
 * - Following the approach of the published replication files by the authors, the labor disutility parameter, which
 *      is unspecified in the paper is set so that labor is 1/3 in the benchmark case; this normalization is relevant
 *      for the definition of welfare
 * - The technology parameter, which is also left unspecified in the published version is set to in steady state, which 
 *      is the natural normalization also used in the official replication files
 * - For the business cycle moments, the technology shock variance is actually 0.45^2 (not 0.45 as reported in the paper), i.e. 
 *      it is consistent with the number reported in Smets/Wouters (2007). Moreover, business cycle moments rely on a Taylor rule 
 *      with interest rate smoothing of 0, inflation feedback of 1.5, and output feedback of 0.5/4.
 * - As described in footnote 54, the determinacy region in Figure 11 is actually the "determinacy and stability region", i.e. it does not distinguish
 *      whether the Blanchard-Kahn conditions fail because of too many unstable roots (instability, info==3) or because of too few unstable roots (indeterminacy, info==4).
 *      The is replicated in Dynare but not distinguishing between the error codes 3 and 4 returned by resol.m.
 * - Changing the risk aversion parameter from the current log-utility specification requires manually changing the definition of the utility function in 
 *      equation 12 and in the steady_state_model-block. Simply replace the log-utility definition by the commented general CRRA definition.
 *
 * This implementation was written by Johannes Pfeifer. I thank Guido Ascari for providing important clarifications.
 *
 * If you spot mistakes, email me at jpfeifer@gmx.de
 *
 * Please note that the following copyright notice only applies to this Dynare 
 * implementation of the model.
 */

/*
 * Copyright (C) 2014-15 Johannes Pfeifer
 *
 * This is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * It is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * For a copy of the GNU General Public License,
 * see <http://www.gnu.org/licenses/>.
 */
set_dynare_seed(240588);

var y $y$ //output
    i $i$ //investment
    pi $\pi$ //inflation
    N $N$ //hours worked
    w $w$ //real wage
    p_star ${p^*}$ //target price
    psi $\psi$  //recursive auxiliary variable 1 price setting 
    phi $\phi$ //recursive auxiliary variable 2 price setting 
    A $A$ //TFP
    MC_real $MC$ //real marginal costs 
    real_interest $r$ //real interest rate
    zeta $\zeta$ //preference shock
    s $s$ //price dispersion term
    v $\nu$ //monetary policy shock
    A_tilde ${\tilde A}$ //"effective aggregate productivity"
    Utility $U$ //lifetime utility, recursively defined
    Average_markup 
    Marginal_markup 
    price_adjustment_gap
    y_gap;

varexo e_v e_a e_zeta;

parameters trend_inflation 
    beta $\beta$ //discount factor 
    alpha $\alpha$ //capital share
    phi_par $\varphi$ //Frisch elasticity
    theta $\theta$ //Calvo parameter
    sigma $\sigma$ //Risk aversion
    epsilon $\varepsilon$ //Elasticity of substitution
    Pi_bar ${\bar \pi}$ //gross quarterly steady state inflation
    rho_v ${\rho_\nu}$ //autocorrelation of monetary shock
    rho_a ${\rho_a}$ //autocorrelation of technology shock
    rho_zeta ${\rho_\zeta}$ //autocorrelation of preference shock
    phi_pi ${\phi_\pi}$ //Taylor rule feedback inflation
    phi_y ${\phi_y}$ //Taylor rule output
    Y_bar ${\bar Y}$ //steady state output, set in steady state model block
    var_rho ${\varrho}$ //degree of indexing
    i_bar ${\bar i}$ //steady state interest rate, set in steady state model block
    d_n ${d_n}$ //labor disutility parameter
    rho_i ${\rho_i}$; //interest rate smoothing parameter


%fix labor to 1/3 in zero trend inflation steady state with Frisch elasticity of 1#
beta_ss = 0.975;
alpha_ss = 0;
theta_ss = 0.75;
epsilon_ss = 3.8;
sigma_ss = 1; %different utility than log case implies that model utility function and its steady state must be manually changed
phi_par_ss=1;
var_rho_ss = 0;
trend_inflation_ss=0;

%%compute labor disutility parameter under benchmark of 0 steady state
%%inflation
Pi_bar = (1+0/100)^(1/4); %set Pi_bar to reflect quarterly inflation
p_star_ss=((1-theta_ss*Pi_bar^((epsilon_ss-1)*(1-var_rho_ss)))/(1-theta_ss))^(1/(1-epsilon_ss));
s_ss=(1-theta_ss)/(1-theta_ss*Pi_bar^((epsilon_ss*(1-var_rho_ss))/(1-alpha_ss)))*p_star_ss^(-epsilon_ss/(1^-alpha_ss));
N_ss=1/3;
y_ss=(N_ss/s_ss)^(1-alpha_ss);
A_ss=1;
phi_ss=y_ss^(1-sigma_ss)/(1-theta_ss*beta_ss*Pi_bar^((epsilon_ss-1)*(1-var_rho_ss)));
psi_ss=p_star_ss^(1+epsilon_ss*alpha_ss/(1-alpha_ss))*phi_ss/(epsilon_ss/((epsilon_ss-1)*(1-alpha_ss)));
w_ss=psi_ss*(1-theta_ss*beta_ss*Pi_bar^((epsilon_ss*(1-var_rho_ss))/(1-alpha_ss)))/(A_ss^(-1/(1-alpha_ss))*y_ss^(1/(1-alpha_ss)-sigma_ss));
d_n=w_ss/(N_ss^phi_par_ss*y_ss^sigma_ss);


trend_inflation=0; %2, 4, or 6

%set according to FN36
beta = 0.975;
alpha = 1/3;
theta = 0.75;
epsilon = 3.8;
sigma = 1; %different utility than log case implies that model utility function and its steady state must be manually changed

rho_v = .65;
rho_a = .65;
rho_zeta = 0;
phi_par =1;
phi_pi = 1.5;
phi_y = 0.5/4;
rho_i=0;   
var_rho = 0;

model;
//1. Euler equation
1/(exp(y)^(sigma)) = beta*(1+exp(i))/(exp(pi(+1))*(exp(y(+1))^(sigma)));
//2. Labor FOC
exp(w) = d_n*exp(zeta)*(exp(N)^phi_par)*(exp(y)^sigma);
//3. Optimal price
exp(p_star) = ((1-theta*(exp(pi(-1))^((1-epsilon)*var_rho))*(exp(pi)^(epsilon-1)))/(1-theta))^(1/(1-epsilon));
//4. FOC price setting
(exp(p_star))^(1+((epsilon*alpha)/(1-alpha))) = (epsilon/((epsilon-1)*(1-alpha)))*exp(psi)/exp(phi);
//5. Recursive LOM price setting for psi
exp(psi) = exp(w)*((exp(A))^(-1/(1-alpha)))*(exp(y)^((1/(1-alpha))-sigma))
            +theta*beta*(exp(pi))^((-var_rho*epsilon)/(1-alpha))*exp(pi(+1))^(epsilon/(1-alpha))*exp(psi(+1));
//6. Recursive LOM price setting for phi
exp(phi) = exp(y)^(1-sigma)+theta*beta*exp(pi)^(var_rho*(1-epsilon))*exp(pi(+1))^(epsilon-1)*exp(phi(+1));
//7. Aggregate production function
exp(N)=exp(s)*(exp(y)/exp(A))^(1/(1-alpha));
//8. LOM price dispersion
exp(s) = (1-theta)*exp(p_star)^(-epsilon/(1-alpha))
        +theta*exp(pi(-1))^((-epsilon*var_rho)/(1-alpha))*exp(pi)^(epsilon/(1-alpha))*exp(s(-1));
//9. Monetary policy rule; reflects FN69
(1+exp(i))/(1+i_bar)=((1+exp(i(-1)))/(1+i_bar))^rho_i*((exp(pi)/Pi_bar)^(phi_pi)*(exp(y)/Y_bar)^(phi_y))^(1-rho_i)*exp(v);

//10. Definition real marginal costs
exp(MC_real)=1/(1-alpha)*exp(w)*exp(A)^(1/(alpha-1))*exp(y)^(alpha/(1-alpha));
//11. Definition real interest rate
exp(real_interest)=(1+exp(i))/(exp(pi(+1)));
//12. Define utility, do not log it as it can be negative; this is the log case
Utility=y-d_n*exp(zeta)*exp(N)^(1+phi_par)/(1+phi_par)+beta*Utility(+1);
// Utility=exp(y)^(1-sigma)/(1-sigma)-d_n*exp(zeta)*exp(N)^(1+phi_par)/(1+phi_par)+beta*Utility(+1);
//13. Monetary shock
v = rho_v*v(-1) + e_v;
//14. Technology shock
A = rho_a*A(-1) + e_a;
//15. Preference shock
zeta = rho_zeta*zeta(-1) + e_zeta;

exp(A_tilde)=exp(A)/exp(s);
exp(Average_markup)=1/exp(MC_real);
exp(Marginal_markup)=exp(p_star)/exp(MC_real);
exp(price_adjustment_gap)=1/exp(p_star);
exp(y_gap) = (exp(y)/Y_bar);
end;

steady_state_model;
Pi_bar = (1+trend_inflation/100)^(1/4); %set Pi_bar to reflect quarterly inflation
v = 0;
A = 1;
zeta=0;
pi=Pi_bar;
i=1/beta*Pi_bar-1;
i_bar=i;
p_star=((1-theta*Pi_bar^((epsilon-1)*(1-var_rho)))/(1-theta))^(1/(1-epsilon));
s=(1-theta)/(1-theta*Pi_bar^((epsilon*(1-var_rho))/(1-alpha)))*p_star^(-epsilon/(1^-alpha));
y=(p_star^(1+(epsilon*alpha)/(1-alpha))*(epsilon/((epsilon-1)*(1-alpha))*((1-beta*theta*Pi_bar^((epsilon-1)*(1-var_rho)))/(1-beta*theta*Pi_bar^(epsilon*(1-var_rho)/(1-alpha))))*d_n*s^phi_par)^(-1))^((1-alpha)/(phi_par+1));
N=s*y^(1/(1-alpha));
Y_bar=y;
phi=y^(1-sigma)/(1-theta*beta*Pi_bar^((epsilon-1)*(1-var_rho)));
psi=p_star^(1+epsilon*alpha/(1-alpha))*phi/(epsilon/((epsilon-1)*(1-alpha)));
//w=psi*(1-theta*beta*Pi_bar^((epsilon*(1-var_rho))/(1-alpha)))/(A^(-1/(1-alpha))*y^(1/(1-alpha)-sigma));
w=d_n*N^phi_par*y^sigma;
MC_real=1/(1-alpha)*w*A^(1/(alpha-1))*y^(alpha/(1-alpha));
Average_markup=1/MC_real;
Marginal_markup=p_star/MC_real;
real_interest=(1+i)/pi;
price_adjustment_gap=1/p_star;
A_tilde=A/s;
Utility=(1-beta)^(-1)*(log(y)-d_n*N^(1+phi_par)/(1+phi_par));
// Utility=(1-beta)^(-1)*(y^(1-sigma)/(1-sigma)-N^(1-phi_par)/(1+phi_par)); %needed for non-log case
A=log(A);
i=log(i);
p_star=log(p_star);
pi=log(pi);
s=log(s);
y=log(y);
phi=log(phi);
psi=log(psi);
w=log(w);
N=log(N);
MC_real=log(MC_real);
real_interest=log(real_interest);
A_tilde=log(A_tilde);
Average_markup=log(Average_markup);
Marginal_markup=log(Marginal_markup);
price_adjustment_gap=log(price_adjustment_gap);
end;

options_.qz_criterium = 1+1e-6; //make sure the option is set

steady;
check;

shocks;          
var e_v; stderr 1;   
var e_a=0.25^2;
var e_zeta; stderr 0;
end;

stoch_simul(order=1,
			solve_algo=2,
			irf = 30,
			periods = 500000,
			drop = 100000,
			replic = 2500) y_gap pi;

verbatim;
save('ascardone_pi', 'pi', '-v6');