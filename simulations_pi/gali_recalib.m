%
% Status : main Dynare file
%
% Warning : this file is generated automatically by Dynare
%           from model file (.mod)

if isoctave || matlab_ver_less_than('8.6')
    clear all
else
    clearvars -global
    clear_persistent_variables(fileparts(which('dynare')), false)
end
tic0 = tic;
% Save empty dates and dseries objects in memory.
dates('initialize');
dseries('initialize');
% Define global variables.
global M_ options_ oo_ estim_params_ bayestopt_ dataset_ dataset_info estimation_info ys0_ ex0_
options_ = [];
M_.fname = 'gali_recalib';
M_.dynare_version = '4.5.1';
oo_.dynare_version = '4.5.1';
options_.dynare_version = '4.5.1';
%
% Some global variables initialization
%
global_initialization;
diary off;
options_.nograph = 1;
M_.exo_names = 'eps_a';
M_.exo_names_tex = '{\varepsilon_a}';
M_.exo_names_long = 'technology shock';
M_.exo_names = char(M_.exo_names, 'eps_nu');
M_.exo_names_tex = char(M_.exo_names_tex, '{\varepsilon_\nu}');
M_.exo_names_long = char(M_.exo_names_long, 'monetary policy shock');
M_.endo_names = 'pi';
M_.endo_names_tex = '{\pi}';
M_.endo_names_long = 'inflation';
M_.endo_names = char(M_.endo_names, 'y_gap');
M_.endo_names_tex = char(M_.endo_names_tex, '{\tilde y}');
M_.endo_names_long = char(M_.endo_names_long, 'output gap');
M_.endo_names = char(M_.endo_names, 'y_nat');
M_.endo_names_tex = char(M_.endo_names_tex, '{y^{nat}}');
M_.endo_names_long = char(M_.endo_names_long, 'natural output');
M_.endo_names = char(M_.endo_names, 'y');
M_.endo_names_tex = char(M_.endo_names_tex, '{y}');
M_.endo_names_long = char(M_.endo_names_long, 'output');
M_.endo_names = char(M_.endo_names, 'r_nat');
M_.endo_names_tex = char(M_.endo_names_tex, '{r^{nat}}');
M_.endo_names_long = char(M_.endo_names_long, 'natural interest rate');
M_.endo_names = char(M_.endo_names, 'r_real');
M_.endo_names_tex = char(M_.endo_names_tex, '{r^r}');
M_.endo_names_long = char(M_.endo_names_long, '//real interest rate');
M_.endo_names = char(M_.endo_names, 'i');
M_.endo_names_tex = char(M_.endo_names_tex, '{i}');
M_.endo_names_long = char(M_.endo_names_long, 'nominal interrst rate');
M_.endo_names = char(M_.endo_names, 'n');
M_.endo_names_tex = char(M_.endo_names_tex, '{n}');
M_.endo_names_long = char(M_.endo_names_long, 'hours worked');
M_.endo_names = char(M_.endo_names, 'm_real');
M_.endo_names_tex = char(M_.endo_names_tex, '{m-p}');
M_.endo_names_long = char(M_.endo_names_long, 'real money stock');
M_.endo_names = char(M_.endo_names, 'm_growth_ann');
M_.endo_names_tex = char(M_.endo_names_tex, '{\Delta m}');
M_.endo_names_long = char(M_.endo_names_long, 'money growth annualized');
M_.endo_names = char(M_.endo_names, 'nu');
M_.endo_names_tex = char(M_.endo_names_tex, '{\nu}');
M_.endo_names_long = char(M_.endo_names_long, 'AR(1) monetary policy shock process');
M_.endo_names = char(M_.endo_names, 'a');
M_.endo_names_tex = char(M_.endo_names_tex, '{a}');
M_.endo_names_long = char(M_.endo_names_long, 'AR(1) technology shock process');
M_.endo_names = char(M_.endo_names, 'r_real_ann');
M_.endo_names_tex = char(M_.endo_names_tex, '{r^{r,ann}}');
M_.endo_names_long = char(M_.endo_names_long, 'annualized real interest rate');
M_.endo_names = char(M_.endo_names, 'i_ann');
M_.endo_names_tex = char(M_.endo_names_tex, '{i^{ann}}');
M_.endo_names_long = char(M_.endo_names_long, 'annualized nominal interest rate');
M_.endo_names = char(M_.endo_names, 'r_nat_ann');
M_.endo_names_tex = char(M_.endo_names_tex, '{r^{nat,ann}}');
M_.endo_names_long = char(M_.endo_names_long, 'annualized natural interest rate');
M_.endo_names = char(M_.endo_names, 'pi_ann');
M_.endo_names_tex = char(M_.endo_names_tex, '{\pi^{ann}}');
M_.endo_names_long = char(M_.endo_names_long, 'annualized inflation rate');
M_.endo_partitions = struct();
M_.param_names = 'alppha';
M_.param_names_tex = '{\alpha}';
M_.param_names_long = 'capital share';
M_.param_names = char(M_.param_names, 'betta');
M_.param_names_tex = char(M_.param_names_tex, '{\beta}');
M_.param_names_long = char(M_.param_names_long, 'discount factor');
M_.param_names = char(M_.param_names, 'rho_a');
M_.param_names_tex = char(M_.param_names_tex, '{\rho_a}');
M_.param_names_long = char(M_.param_names_long, 'autocorrelation technology shock');
M_.param_names = char(M_.param_names, 'rho_nu');
M_.param_names_tex = char(M_.param_names_tex, '{\rho_{\nu}}');
M_.param_names_long = char(M_.param_names_long, 'autocorrelation monetary policy shock');
M_.param_names = char(M_.param_names, 'siggma');
M_.param_names_tex = char(M_.param_names_tex, '{\sigma}');
M_.param_names_long = char(M_.param_names_long, 'log utility');
M_.param_names = char(M_.param_names, 'phi');
M_.param_names_tex = char(M_.param_names_tex, '{\phi}');
M_.param_names_long = char(M_.param_names_long, 'unitary Frisch elasticity');
M_.param_names = char(M_.param_names, 'phi_pi');
M_.param_names_tex = char(M_.param_names_tex, '{\phi_{\pi}}');
M_.param_names_long = char(M_.param_names_long, 'inflation feedback Taylor Rule');
M_.param_names = char(M_.param_names, 'phi_y');
M_.param_names_tex = char(M_.param_names_tex, '{\phi_{y}}');
M_.param_names_long = char(M_.param_names_long, 'output feedback Taylor Rule');
M_.param_names = char(M_.param_names, 'eta');
M_.param_names_tex = char(M_.param_names_tex, '{\eta}');
M_.param_names_long = char(M_.param_names_long, 'semi-elasticity of money demand');
M_.param_names = char(M_.param_names, 'epsilon');
M_.param_names_tex = char(M_.param_names_tex, '{\epsilon}');
M_.param_names_long = char(M_.param_names_long, 'demand elasticity');
M_.param_names = char(M_.param_names, 'theta');
M_.param_names_tex = char(M_.param_names_tex, '{\theta}');
M_.param_names_long = char(M_.param_names_long, 'Calvo parameter');
M_.param_partitions = struct();
M_.exo_det_nbr = 0;
M_.exo_nbr = 2;
M_.endo_nbr = 16;
M_.param_nbr = 11;
M_.orig_endo_nbr = 16;
M_.aux_vars = [];
M_.Sigma_e = zeros(2, 2);
M_.Correlation_matrix = eye(2, 2);
M_.H = 0;
M_.Correlation_matrix_ME = 1;
M_.sigma_e_is_diagonal = 1;
M_.det_shocks = [];
options_.linear = 1;
options_.block=0;
options_.bytecode=0;
options_.use_dll=0;
M_.hessian_eq_zero = 1;
erase_compiled_function('gali_recalib_static');
erase_compiled_function('gali_recalib_dynamic');
M_.orig_eq_nbr = 16;
M_.eq_nbr = 16;
M_.ramsey_eq_nbr = 0;
M_.lead_lag_incidence = [
 0 5 21;
 0 6 22;
 0 7 0;
 1 8 0;
 0 9 0;
 0 10 0;
 2 11 0;
 0 12 0;
 0 13 0;
 0 14 0;
 3 15 0;
 4 16 23;
 0 17 0;
 0 18 0;
 0 19 0;
 0 20 0;]';
M_.nstatic = 10;
M_.nfwrd   = 2;
M_.npred   = 3;
M_.nboth   = 1;
M_.nsfwrd   = 3;
M_.nspred   = 4;
M_.ndynamic   = 6;
M_.equations_tags = {
};
M_.static_and_dynamic_models_differ = 0;
M_.exo_names_orig_ord = [1:2];
M_.maximum_lag = 1;
M_.maximum_lead = 1;
M_.maximum_endo_lag = 1;
M_.maximum_endo_lead = 1;
oo_.steady_state = zeros(16, 1);
M_.maximum_exo_lag = 0;
M_.maximum_exo_lead = 0;
oo_.exo_steady_state = zeros(2, 1);
M_.params = NaN(11, 1);
M_.NNZDerivatives = [49; -1; -1];
set_dynare_seed(240588);
M_.params( 5 ) = 5;
siggma = M_.params( 5 );
M_.params( 6 ) = 1;
phi = M_.params( 6 );
M_.params( 8 ) = 0.125;
phi_y = M_.params( 8 );
M_.params( 11 ) = 0.75;
theta = M_.params( 11 );
M_.params( 4 ) = 0.65;
rho_nu = M_.params( 4 );
M_.params( 3 ) = 0.65;
rho_a = M_.params( 3 );
M_.params( 2 ) = 0.975;
betta = M_.params( 2 );
M_.params( 9 ) = 4;
eta = M_.params( 9 );
M_.params( 1 ) = 0.3333333333333333;
alppha = M_.params( 1 );
M_.params( 10 ) = 3.8;
epsilon = M_.params( 10 );
M_.params( 7 ) = 180;
phi_pi = M_.params( 7 );
%
% SHOCKS instructions
%
M_.exo_det_length = 0;
M_.Sigma_e(1, 1) = 1;
M_.Sigma_e(2, 2) = 0.0625;
resid(1);
steady;
oo_.dr.eigval = check(M_,options_,oo_);
options_.drop = 100000;
options_.irf = 30;
options_.order = 1;
options_.periods = 500000;
options_.replic = 2500;
options_.solve_algo = 2;
var_list_ = char('y_gap','pi','i');
info = stoch_simul(var_list_);
len=options_.irf;
free_ax = [1 inf -inf inf];
irf_tfp = figure('Name', 'TFP shock', 'visible', 'off');
subplot(3,1,1);
plot(oo_.irfs.y_gap_eps_a, 'black', 'LineWidth', 4);
hold on;
line([0 len], [0 0], 'Color', 'red', 'LineWidth', 4);
axis([1 inf -.6 .1]);
    axis(free_ax);
ylabel('Output gap');
hold off;
subplot(3,1,2);
plot(oo_.irfs.pi_eps_a, 'black', 'LineWidth', 4);
hold on;
line([0 len], [0 0], 'Color', 'red', 'LineWidth', 4);
axis([1 inf -.25 .05]);
    axis(free_ax);
ylabel('Inflation');
hold off;
subplot(3,1,3);
plot(oo_.irfs.i_eps_a, 'black', 'LineWidth', 4);
hold on;
line([0 len], [0 0], 'Color', 'red', 'LineWidth', 4);
axis([1 inf -.45 .05]);
    axis(free_ax);
ylabel('Interest rate');
hold off;
irf_mon = figure('Name', 'Monetary policy shock', 'visible', 'off');
subplot(3,1,1);
plot(oo_.irfs.y_gap_eps_nu, 'black', 'LineWidth', 4);
hold on;
line([0 len], [0 0], 'Color', 'red', 'LineWidth', 4);
axis([1 inf -.2 .015]);
    axis(free_ax);
ylabel('Output gap');
hold off;
subplot(3,1,2);
plot(oo_.irfs.pi_eps_nu, 'black', 'LineWidth', 4);
hold on;
line([0 len], [0 0], 'Color', 'red', 'LineWidth', 4);
axis([1 inf -.12 .015]);
    axis(free_ax);
ylabel('Inflation');
hold off;
subplot(3,1,3);
plot(oo_.irfs.i_eps_nu, 'black', 'LineWidth', 4);
hold on;
line([0 len], [0 0], 'Color', 'red', 'LineWidth', 4);
axis([1 inf 0 .2]);
    axis(free_ax);
ylabel('Interest rate');
hold off;
print(irf_tfp, 'nkdsge_aggressive_tfp', '-dpdf', '-fillpage');
print(irf_mon, 'nkdsge_aggressive_mp', '-dpdf', '-fillpage');
clear len;
clear irf_tfp;
clear irf_mon;
clear free_ax;
resid(1);
save('gali_recalib_results.mat', 'oo_', 'M_', 'options_');
if exist('estim_params_', 'var') == 1
  save('gali_recalib_results.mat', 'estim_params_', '-append');
end
if exist('bayestopt_', 'var') == 1
  save('gali_recalib_results.mat', 'bayestopt_', '-append');
end
if exist('dataset_', 'var') == 1
  save('gali_recalib_results.mat', 'dataset_', '-append');
end
if exist('estimation_info', 'var') == 1
  save('gali_recalib_results.mat', 'estimation_info', '-append');
end
if exist('dataset_info', 'var') == 1
  save('gali_recalib_results.mat', 'dataset_info', '-append');
end
if exist('oo_recursive_', 'var') == 1
  save('gali_recalib_results.mat', 'oo_recursive_', '-append');
end


disp(['Total computing time : ' dynsec2hms(toc(tic0)) ]);
if ~isempty(lastwarn)
  disp('Note: warning(s) encountered in MATLAB/Octave code')
end
