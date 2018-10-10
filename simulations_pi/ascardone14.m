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
M_.fname = 'ascardone14';
M_.dynare_version = '4.5.1';
oo_.dynare_version = '4.5.1';
options_.dynare_version = '4.5.1';
%
% Some global variables initialization
%
global_initialization;
diary off;
options_.nograph = 1;
M_.exo_names = 'e_v';
M_.exo_names_tex = 'e\_v';
M_.exo_names_long = 'e_v';
M_.exo_names = char(M_.exo_names, 'e_a');
M_.exo_names_tex = char(M_.exo_names_tex, 'e\_a');
M_.exo_names_long = char(M_.exo_names_long, 'e_a');
M_.exo_names = char(M_.exo_names, 'e_zeta');
M_.exo_names_tex = char(M_.exo_names_tex, 'e\_zeta');
M_.exo_names_long = char(M_.exo_names_long, 'e_zeta');
M_.endo_names = 'y';
M_.endo_names_tex = 'y';
M_.endo_names_long = 'y';
M_.endo_names = char(M_.endo_names, 'i');
M_.endo_names_tex = char(M_.endo_names_tex, 'i');
M_.endo_names_long = char(M_.endo_names_long, 'i');
M_.endo_names = char(M_.endo_names, 'pi');
M_.endo_names_tex = char(M_.endo_names_tex, '\pi');
M_.endo_names_long = char(M_.endo_names_long, 'pi');
M_.endo_names = char(M_.endo_names, 'N');
M_.endo_names_tex = char(M_.endo_names_tex, 'N');
M_.endo_names_long = char(M_.endo_names_long, 'N');
M_.endo_names = char(M_.endo_names, 'w');
M_.endo_names_tex = char(M_.endo_names_tex, 'w');
M_.endo_names_long = char(M_.endo_names_long, 'w');
M_.endo_names = char(M_.endo_names, 'p_star');
M_.endo_names_tex = char(M_.endo_names_tex, '{p^*}');
M_.endo_names_long = char(M_.endo_names_long, 'p_star');
M_.endo_names = char(M_.endo_names, 'psi');
M_.endo_names_tex = char(M_.endo_names_tex, '\psi');
M_.endo_names_long = char(M_.endo_names_long, 'psi');
M_.endo_names = char(M_.endo_names, 'phi');
M_.endo_names_tex = char(M_.endo_names_tex, '\phi');
M_.endo_names_long = char(M_.endo_names_long, 'phi');
M_.endo_names = char(M_.endo_names, 'A');
M_.endo_names_tex = char(M_.endo_names_tex, 'A');
M_.endo_names_long = char(M_.endo_names_long, 'A');
M_.endo_names = char(M_.endo_names, 'MC_real');
M_.endo_names_tex = char(M_.endo_names_tex, 'MC');
M_.endo_names_long = char(M_.endo_names_long, 'MC_real');
M_.endo_names = char(M_.endo_names, 'real_interest');
M_.endo_names_tex = char(M_.endo_names_tex, 'r');
M_.endo_names_long = char(M_.endo_names_long, 'real_interest');
M_.endo_names = char(M_.endo_names, 'zeta');
M_.endo_names_tex = char(M_.endo_names_tex, '\zeta');
M_.endo_names_long = char(M_.endo_names_long, 'zeta');
M_.endo_names = char(M_.endo_names, 's');
M_.endo_names_tex = char(M_.endo_names_tex, 's');
M_.endo_names_long = char(M_.endo_names_long, 's');
M_.endo_names = char(M_.endo_names, 'v');
M_.endo_names_tex = char(M_.endo_names_tex, '\nu');
M_.endo_names_long = char(M_.endo_names_long, 'v');
M_.endo_names = char(M_.endo_names, 'A_tilde');
M_.endo_names_tex = char(M_.endo_names_tex, '{\tilde A}');
M_.endo_names_long = char(M_.endo_names_long, 'A_tilde');
M_.endo_names = char(M_.endo_names, 'Utility');
M_.endo_names_tex = char(M_.endo_names_tex, 'U');
M_.endo_names_long = char(M_.endo_names_long, 'Utility');
M_.endo_names = char(M_.endo_names, 'Average_markup');
M_.endo_names_tex = char(M_.endo_names_tex, 'Average\_markup');
M_.endo_names_long = char(M_.endo_names_long, 'Average_markup');
M_.endo_names = char(M_.endo_names, 'Marginal_markup');
M_.endo_names_tex = char(M_.endo_names_tex, 'Marginal\_markup');
M_.endo_names_long = char(M_.endo_names_long, 'Marginal_markup');
M_.endo_names = char(M_.endo_names, 'price_adjustment_gap');
M_.endo_names_tex = char(M_.endo_names_tex, 'price\_adjustment\_gap');
M_.endo_names_long = char(M_.endo_names_long, 'price_adjustment_gap');
M_.endo_names = char(M_.endo_names, 'y_gap');
M_.endo_names_tex = char(M_.endo_names_tex, 'y\_gap');
M_.endo_names_long = char(M_.endo_names_long, 'y_gap');
M_.endo_partitions = struct();
M_.param_names = 'trend_inflation';
M_.param_names_tex = 'trend\_inflation';
M_.param_names_long = 'trend_inflation';
M_.param_names = char(M_.param_names, 'beta');
M_.param_names_tex = char(M_.param_names_tex, '\beta');
M_.param_names_long = char(M_.param_names_long, 'beta');
M_.param_names = char(M_.param_names, 'alpha');
M_.param_names_tex = char(M_.param_names_tex, '\alpha');
M_.param_names_long = char(M_.param_names_long, 'alpha');
M_.param_names = char(M_.param_names, 'phi_par');
M_.param_names_tex = char(M_.param_names_tex, '\varphi');
M_.param_names_long = char(M_.param_names_long, 'phi_par');
M_.param_names = char(M_.param_names, 'theta');
M_.param_names_tex = char(M_.param_names_tex, '\theta');
M_.param_names_long = char(M_.param_names_long, 'theta');
M_.param_names = char(M_.param_names, 'sigma');
M_.param_names_tex = char(M_.param_names_tex, '\sigma');
M_.param_names_long = char(M_.param_names_long, 'sigma');
M_.param_names = char(M_.param_names, 'epsilon');
M_.param_names_tex = char(M_.param_names_tex, '\varepsilon');
M_.param_names_long = char(M_.param_names_long, 'epsilon');
M_.param_names = char(M_.param_names, 'Pi_bar');
M_.param_names_tex = char(M_.param_names_tex, '{\bar \pi}');
M_.param_names_long = char(M_.param_names_long, 'Pi_bar');
M_.param_names = char(M_.param_names, 'rho_v');
M_.param_names_tex = char(M_.param_names_tex, '{\rho_\nu}');
M_.param_names_long = char(M_.param_names_long, 'rho_v');
M_.param_names = char(M_.param_names, 'rho_a');
M_.param_names_tex = char(M_.param_names_tex, '{\rho_a}');
M_.param_names_long = char(M_.param_names_long, 'rho_a');
M_.param_names = char(M_.param_names, 'rho_zeta');
M_.param_names_tex = char(M_.param_names_tex, '{\rho_\zeta}');
M_.param_names_long = char(M_.param_names_long, 'rho_zeta');
M_.param_names = char(M_.param_names, 'phi_pi');
M_.param_names_tex = char(M_.param_names_tex, '{\phi_\pi}');
M_.param_names_long = char(M_.param_names_long, 'phi_pi');
M_.param_names = char(M_.param_names, 'phi_y');
M_.param_names_tex = char(M_.param_names_tex, '{\phi_y}');
M_.param_names_long = char(M_.param_names_long, 'phi_y');
M_.param_names = char(M_.param_names, 'Y_bar');
M_.param_names_tex = char(M_.param_names_tex, '{\bar Y}');
M_.param_names_long = char(M_.param_names_long, 'Y_bar');
M_.param_names = char(M_.param_names, 'var_rho');
M_.param_names_tex = char(M_.param_names_tex, '{\varrho}');
M_.param_names_long = char(M_.param_names_long, 'var_rho');
M_.param_names = char(M_.param_names, 'i_bar');
M_.param_names_tex = char(M_.param_names_tex, '{\bar i}');
M_.param_names_long = char(M_.param_names_long, 'i_bar');
M_.param_names = char(M_.param_names, 'd_n');
M_.param_names_tex = char(M_.param_names_tex, '{d_n}');
M_.param_names_long = char(M_.param_names_long, 'd_n');
M_.param_names = char(M_.param_names, 'rho_i');
M_.param_names_tex = char(M_.param_names_tex, '{\rho_i}');
M_.param_names_long = char(M_.param_names_long, 'rho_i');
M_.param_partitions = struct();
M_.exo_det_nbr = 0;
M_.exo_nbr = 3;
M_.endo_nbr = 20;
M_.param_nbr = 18;
M_.orig_endo_nbr = 20;
M_.aux_vars = [];
M_.Sigma_e = zeros(3, 3);
M_.Correlation_matrix = eye(3, 3);
M_.H = 0;
M_.Correlation_matrix_ME = 1;
M_.sigma_e_is_diagonal = 1;
M_.det_shocks = [];
options_.block=0;
options_.bytecode=0;
options_.use_dll=0;
M_.hessian_eq_zero = 1;
erase_compiled_function('ascardone14_static');
erase_compiled_function('ascardone14_dynamic');
M_.orig_eq_nbr = 20;
M_.eq_nbr = 20;
M_.ramsey_eq_nbr = 0;
M_.lead_lag_incidence = [
 0 7 27;
 1 8 0;
 2 9 28;
 0 10 0;
 0 11 0;
 0 12 0;
 0 13 29;
 0 14 30;
 3 15 0;
 0 16 0;
 0 17 0;
 4 18 0;
 5 19 0;
 6 20 0;
 0 21 0;
 0 22 31;
 0 23 0;
 0 24 0;
 0 25 0;
 0 26 0;]';
M_.nstatic = 10;
M_.nfwrd   = 4;
M_.npred   = 5;
M_.nboth   = 1;
M_.nsfwrd   = 5;
M_.nspred   = 6;
M_.ndynamic   = 10;
M_.equations_tags = {
};
M_.static_and_dynamic_models_differ = 0;
M_.exo_names_orig_ord = [1:3];
M_.maximum_lag = 1;
M_.maximum_lead = 1;
M_.maximum_endo_lag = 1;
M_.maximum_endo_lead = 1;
oo_.steady_state = zeros(20, 1);
M_.maximum_exo_lag = 0;
M_.maximum_exo_lead = 0;
oo_.exo_steady_state = zeros(3, 1);
M_.params = NaN(18, 1);
M_.NNZDerivatives = [73; -1; -1];
set_dynare_seed(240588);
beta_ss = 0.975;
alpha_ss = 0;
theta_ss = 0.75;
epsilon_ss = 3.8;
sigma_ss = 1; 
phi_par_ss=1;
var_rho_ss = 0;
trend_inflation_ss=0;
M_.params( 8 ) = 1;
Pi_bar = M_.params( 8 );
p_star_ss=((1-theta_ss*Pi_bar^((epsilon_ss-1)*(1-var_rho_ss)))/(1-theta_ss))^(1/(1-epsilon_ss));
s_ss=(1-theta_ss)/(1-theta_ss*Pi_bar^((epsilon_ss*(1-var_rho_ss))/(1-alpha_ss)))*p_star_ss^(-epsilon_ss/(1^-alpha_ss));
N_ss=1/3;
y_ss=(N_ss/s_ss)^(1-alpha_ss);
A_ss=1;
phi_ss=y_ss^(1-sigma_ss)/(1-theta_ss*beta_ss*Pi_bar^((epsilon_ss-1)*(1-var_rho_ss)));
psi_ss=p_star_ss^(1+epsilon_ss*alpha_ss/(1-alpha_ss))*phi_ss/(epsilon_ss/((epsilon_ss-1)*(1-alpha_ss)));
w_ss=psi_ss*(1-theta_ss*beta_ss*Pi_bar^((epsilon_ss*(1-var_rho_ss))/(1-alpha_ss)))/(A_ss^(-1/(1-alpha_ss))*y_ss^(1/(1-alpha_ss)-sigma_ss));
M_.params( 17 ) = w_ss/(N_ss^phi_par_ss*y_ss^sigma_ss);
d_n = M_.params( 17 );
M_.params( 1 ) = 0;
trend_inflation = M_.params( 1 );
M_.params( 2 ) = 0.975;
beta = M_.params( 2 );
M_.params( 3 ) = 0.3333333333333333;
alpha = M_.params( 3 );
M_.params( 5 ) = 0.75;
theta = M_.params( 5 );
M_.params( 7 ) = 3.8;
epsilon = M_.params( 7 );
M_.params( 6 ) = 1;
sigma = M_.params( 6 );
M_.params( 9 ) = .65;
rho_v = M_.params( 9 );
M_.params( 10 ) = .65;
rho_a = M_.params( 10 );
M_.params( 11 ) = 0;
rho_zeta = M_.params( 11 );
M_.params( 4 ) = 1;
phi_par = M_.params( 4 );
M_.params( 12 ) = 1.5;
phi_pi = M_.params( 12 );
M_.params( 13 ) = 0.125;
phi_y = M_.params( 13 );
M_.params( 18 ) = 0;
rho_i = M_.params( 18 );
M_.params( 15 ) = 0;
var_rho = M_.params( 15 );
options_.qz_criterium = 1+1e-6; 
steady;
oo_.dr.eigval = check(M_,options_,oo_);
%
% SHOCKS instructions
%
M_.exo_det_length = 0;
M_.Sigma_e(2, 2) = 0.0625;
M_.Sigma_e(1, 1) = (1)^2;
M_.Sigma_e(3, 3) = (0)^2;
options_.drop = 100000;
options_.irf = 30;
options_.order = 1;
options_.periods = 500000;
options_.replic = 2500;
options_.solve_algo = 2;
var_list_ = char('y_gap','pi');
info = stoch_simul(var_list_);
save('ascardone_pi', 'pi', '-v6');
options_.qz_criterium = 1+1e-6; 
save('ascardone14_results.mat', 'oo_', 'M_', 'options_');
if exist('estim_params_', 'var') == 1
  save('ascardone14_results.mat', 'estim_params_', '-append');
end
if exist('bayestopt_', 'var') == 1
  save('ascardone14_results.mat', 'bayestopt_', '-append');
end
if exist('dataset_', 'var') == 1
  save('ascardone14_results.mat', 'dataset_', '-append');
end
if exist('estimation_info', 'var') == 1
  save('ascardone14_results.mat', 'estimation_info', '-append');
end
if exist('dataset_info', 'var') == 1
  save('ascardone14_results.mat', 'dataset_info', '-append');
end
if exist('oo_recursive_', 'var') == 1
  save('ascardone14_results.mat', 'oo_recursive_', '-append');
end


disp(['Total computing time : ' dynsec2hms(toc(tic0)) ]);
disp('Note: 17 warning(s) encountered in the preprocessor')
if ~isempty(lastwarn)
  disp('Note: warning(s) encountered in MATLAB/Octave code')
end
