/*
 * This file implements the baseline New Keynesian model of Jordi Gal� (2008): Monetary Policy, Inflation,
 * and the Business Cycle, Princeton University Press, Chapter 3
 *
 * Note that all model variables are expressed in deviations from steady state, i.e. in contrast to
 * to the chapter, both the nominal interest rate and natural output are not in log-levels, but rather mean 0
 *
 * This implementation was written by Johannes Pfeifer. In case you spot mistakes,
 * email me at jpfeifer@gmx.de
 *
 * Please note that the following copyright notice only applies to this Dynare 
 * implementation of the model.
 */

/*
 * Copyright (C) 2013-15 Johannes Pfeifer
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

%define whether to use interest rate or money growth rate rule 

var pi ${\pi}$ (long_name='inflation')
    y_gap ${\tilde y}$ (long_name='output gap')
    y_nat ${y^{nat}}$ (long_name='natural output')      //(in contrast to the textbook defined in deviation from steady state)
    y ${y}$ (long_name='output')
    r_nat ${r^{nat}}$ (long_name='natural interest rate')
    r_real ${r^r}$ (long_name='//real interest rate')     
    i ${i}$ (long_name='nominal interrst rate')
    n ${n}$ (long_name='hours worked')
    m_real ${m-p}$ (long_name='real money stock')
    m_growth_ann ${\Delta m}$ (long_name='money growth annualized')
    nu ${\nu}$ (long_name='AR(1) monetary policy shock process')    
    a  ${a}$ (long_name='AR(1) technology shock process')
    r_real_ann ${r^{r,ann}}$ (long_name='annualized real interest rate')
    i_ann ${i^{ann}}$ (long_name='annualized nominal interest rate')
    r_nat_ann ${r^{nat,ann}}$ (long_name='annualized natural interest rate')
    pi_ann ${\pi^{ann}}$ (long_name='annualized inflation rate')
    ;     

varexo eps_a ${\varepsilon_a}$   (long_name='technology shock')
       eps_nu ${\varepsilon_\nu}$   (long_name='monetary policy shock')
       ;

parameters alppha ${\alpha}$ (long_name='capital share')
    betta ${\beta}$ (long_name='discount factor')
    rho_a ${\rho_a}$ (long_name='autocorrelation technology shock')
    rho_nu ${\rho_{\nu}}$ (long_name='autocorrelation monetary policy shock')
    siggma ${\sigma}$ (long_name='log utility')
    phi ${\phi}$ (long_name='unitary Frisch elasticity')
    phi_pi ${\phi_{\pi}}$ (long_name='inflation feedback Taylor Rule')
    phi_y ${\phi_{y}}$ (long_name='output feedback Taylor Rule')
    eta ${\eta}$ (long_name='semi-elasticity of money demand')
    epsilon ${\epsilon}$ (long_name='demand elasticity')
    theta ${\theta}$ (long_name='Calvo parameter')
    ;
%----------------------------------------------------------------
% Parametrization, p. 52
%----------------------------------------------------------------
siggma = 5;
phi=1;
phi_y  = .5/4;
theta=3/4;
rho_nu =0.65;
rho_a  = 0.65;
betta = 0.975;
eta  =4;
alppha=1/3;
epsilon=3.8;


% regular calibration
@#if flag_gali==0
phi_pi = 1.8;
@#endif

% lowerbound calibration
@#if flag_gali==1
phi_pi = 1.00000000001; % prev 1.0001
@#endif

% extremely aggressive calibration
@#if flag_gali==2
phi_pi = 180;
@#endif

%----------------------------------------------------------------
% First Order Conditions
%----------------------------------------------------------------

model(linear); 
//Composite parameters
#Omega=(1-alppha)/(1-alppha+alppha*epsilon);  //defined on page 47
#psi_n_ya=(1+phi)/(siggma*(1-alppha)+phi+alppha); //defined on page 48
#lambda=(1-theta)*(1-betta*theta)/theta*Omega; //defined on page 47
#kappa=lambda*(siggma+(phi+alppha)/(1-alppha));  //defined on page 49

//1. New Keynesian Phillips Curve eq. (21)
pi=betta*pi(+1)+kappa*y_gap;
//2. Dynamic IS Curve eq. (22)
y_gap=-1/siggma*(i-pi(+1)-r_nat)+y_gap(+1);
//3. Interest Rate Rule eq. (25)
i=phi_pi*pi+phi_y*y_gap+nu;
//4. Definition natural rate of interest eq. (23)
r_nat=siggma*psi_n_ya*(a(+1)-a);
//5. Definition real interest rate
r_real=i-pi(+1);
//6. Definition natural output, eq. (19)
y_nat=psi_n_ya*a;
//7. Definition output gap
y_gap=y-y_nat;
//8. Monetary policy shock
nu=rho_nu*nu(-1)+eps_nu;
//9. TFP shock
a=rho_a*a(-1)+eps_a;
//10. Production function (eq. 13)
y=a+(1-alppha)*n;
//11. Money growth (derived from eq. (4))
m_growth_ann=4*(y-y(-1)-eta*(i-i(-1))+pi);
//12. Real money demand (eq. 4)
m_real=y-eta*i;


//12. Annualized nominal interest rate
i_ann=4*i;
//13. Annualized real interest rate
r_real_ann=4*r_real;
//14. Annualized natural interest rate
r_nat_ann=4*r_nat;
//15. Annualized inflation
pi_ann=4*pi;
end;

%----------------------------------------------------------------
%  define shock variances
%---------------------------------------------------------------


shocks;
    var eps_nu = 0.25^2; //1 standard deviation shock of 25 basis points, i.e. 1 percentage point annualized
    var eps_a  = 1^2; //unit shock to technology
end;

%----------------------------------------------------------------
%  steady states: all 0 due to linear model
%---------------------------------------------------------------
resid(1);
steady;
check;

%----------------------------------------------------------------
% generate IRFs, replicates Figures 3.1, p. 53 (interest rate rule)
% 3.3, p. 57 (money growth rule)
%----------------------------------------------------------------
stoch_simul(order = 1,
            solve_algo = 2,
            irf=30,
            periods = 500000,
            drop = 100000,
            replic = 2500) y_gap pi i;


verbatim;

len=options_.irf;
free_ax = [1 inf -inf inf];

irf_tfp = figure('Name', 'TFP shock', 'visible', 'off');

subplot(3,1,1);
plot(oo_.irfs.y_gap_eps_a, 'black', 'LineWidth', 1);
hold on;
line([0 len], [0 0], 'Color', 'red', 'LineWidth', 1);
axis([1 inf -.6 .1]);
@#if flag_gali == 2
    axis(free_ax);
@#endif
ylabel('Output gap');
hold off;


subplot(3,1,2);
plot(oo_.irfs.pi_eps_a, 'black', 'LineWidth', 1);
hold on;
line([0 len], [0 0], 'Color', 'red', 'LineWidth', 1);
axis([1 inf -.25 .05]);
@#if flag_gali == 2
    axis(free_ax);
@#endif
ylabel('Inflation');
hold off;

subplot(3,1,3);
plot(oo_.irfs.i_eps_a, 'black', 'LineWidth', 1);
hold on;
line([0 len], [0 0], 'Color', 'red', 'LineWidth', 1);
axis([1 inf -.4 .05]);
@#if flag_gali == 2
    axis(free_ax);
@#endif
ylabel('Interest rate');
hold off;




irf_mon = figure('Name', 'Monetary policy shock', 'visible', 'off');

subplot(3,1,1);
plot(oo_.irfs.y_gap_eps_nu, 'black', 'LineWidth', 1);
hold on;
line([0 len], [0 0], 'Color', 'red', 'LineWidth', 1);
axis([1 inf -.2 .025]);
@#if flag_gali == 2
    axis(free_ax);
@#endif
ylabel('Output gap');
hold off;


subplot(3,1,2);
plot(oo_.irfs.pi_eps_nu, 'black', 'LineWidth', 1);
hold on;
line([0 len], [0 0], 'Color', 'red', 'LineWidth', 1);
axis([1 inf -.1 .05]);
@#if flag_gali == 2
    axis(free_ax);
@#endif
ylabel('Inflation');
hold off;

subplot(3,1,3);
plot(oo_.irfs.i_eps_nu, 'black', 'LineWidth', 1);
hold on;
line([0 len], [0 0], 'Color', 'red', 'LineWidth', 1);
axis([1 inf 0 .15]);
@#if flag_gali == 2
    axis(free_ax);
@#endif
ylabel('Interest rate');
hold off;

@#if flag_gali == 0
print(irf_tfp, 'nkdsge_tfp', '-deps');
print(irf_mon, 'nkdsge_mp', '-deps');
save('gali_pi', 'pi', '-v6');
@#endif


@#if flag_gali == 1
print(irf_tfp, 'nkdsge_accommodative_tfp', '-deps');
print(irf_mon, 'nkdsge_accommodative_mp', '-deps');
@#endif


@#if flag_gali == 2
print(irf_tfp, 'nkdsge_aggressive_tfp', '-deps');
print(irf_mon, 'nkdsge_aggressive_mp', '-deps');
@#endif


clear len;
clear irf_tfp;
clear irf_mon;
clear free_ax;
