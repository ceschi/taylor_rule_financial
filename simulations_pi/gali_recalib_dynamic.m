function [residual, g1, g2, g3] = gali_recalib_dynamic(y, x, params, steady_state, it_)
%
% Status : Computes dynamic model for Dynare
%
% Inputs :
%   y         [#dynamic variables by 1] double    vector of endogenous variables in the order stored
%                                                 in M_.lead_lag_incidence; see the Manual
%   x         [nperiods by M_.exo_nbr] double     matrix of exogenous variables (in declaration order)
%                                                 for all simulation periods
%   steady_state  [M_.endo_nbr by 1] double       vector of steady state values
%   params    [M_.param_nbr by 1] double          vector of parameter values in declaration order
%   it_       scalar double                       time period for exogenous variables for which to evaluate the model
%
% Outputs:
%   residual  [M_.endo_nbr by 1] double    vector of residuals of the dynamic model equations in order of 
%                                          declaration of the equations.
%                                          Dynare may prepend auxiliary equations, see M_.aux_vars
%   g1        [M_.endo_nbr by #dynamic variables] double    Jacobian matrix of the dynamic model equations;
%                                                           rows: equations in order of declaration
%                                                           columns: variables in order stored in M_.lead_lag_incidence followed by the ones in M_.exo_names
%   g2        [M_.endo_nbr by (#dynamic variables)^2] double   Hessian matrix of the dynamic model equations;
%                                                              rows: equations in order of declaration
%                                                              columns: variables in order stored in M_.lead_lag_incidence followed by the ones in M_.exo_names
%   g3        [M_.endo_nbr by (#dynamic variables)^3] double   Third order derivative matrix of the dynamic model equations;
%                                                              rows: equations in order of declaration
%                                                              columns: variables in order stored in M_.lead_lag_incidence followed by the ones in M_.exo_names
%
%
% Warning : this file is generated automatically by Dynare
%           from model file (.mod)

%
% Model equations
%

residual = zeros(16, 1);
Omega__ = (1-params(1))/(1-params(1)+params(1)*params(10));
psi_n_ya__ = (1+params(6))/(params(1)+params(6)+(1-params(1))*params(5));
lambda__ = (1-params(11))*(1-params(11)*params(2))/params(11)*Omega__;
kappa__ = lambda__*(params(5)+(params(1)+params(6))/(1-params(1)));
T43 = (-1)/params(5);
lhs =y(5);
rhs =params(2)*y(21)+kappa__*y(6);
residual(1)= lhs-rhs;
lhs =y(6);
rhs =T43*(y(11)-y(21)-y(9))+y(22);
residual(2)= lhs-rhs;
lhs =y(11);
rhs =y(5)*params(7)+y(6)*params(8)+y(15);
residual(3)= lhs-rhs;
lhs =y(9);
rhs =params(5)*psi_n_ya__*(y(23)-y(16));
residual(4)= lhs-rhs;
lhs =y(10);
rhs =y(11)-y(21);
residual(5)= lhs-rhs;
lhs =y(7);
rhs =psi_n_ya__*y(16);
residual(6)= lhs-rhs;
lhs =y(6);
rhs =y(8)-y(7);
residual(7)= lhs-rhs;
lhs =y(15);
rhs =params(4)*y(3)+x(it_, 2);
residual(8)= lhs-rhs;
lhs =y(16);
rhs =params(3)*y(4)+x(it_, 1);
residual(9)= lhs-rhs;
lhs =y(8);
rhs =y(16)+(1-params(1))*y(12);
residual(10)= lhs-rhs;
lhs =y(14);
rhs =4*(y(5)+y(8)-y(1)-params(9)*(y(11)-y(2)));
residual(11)= lhs-rhs;
lhs =y(13);
rhs =y(8)-y(11)*params(9);
residual(12)= lhs-rhs;
lhs =y(18);
rhs =y(11)*4;
residual(13)= lhs-rhs;
lhs =y(17);
rhs =y(10)*4;
residual(14)= lhs-rhs;
lhs =y(19);
rhs =y(9)*4;
residual(15)= lhs-rhs;
lhs =y(20);
rhs =y(5)*4;
residual(16)= lhs-rhs;
if nargout >= 2,
  g1 = zeros(16, 25);

  %
  % Jacobian matrix
  %

  g1(1,5)=1;
  g1(1,21)=(-params(2));
  g1(1,6)=(-kappa__);
  g1(2,21)=T43;
  g1(2,6)=1;
  g1(2,22)=(-1);
  g1(2,9)=T43;
  g1(2,11)=(-T43);
  g1(3,5)=(-params(7));
  g1(3,6)=(-params(8));
  g1(3,11)=1;
  g1(3,15)=(-1);
  g1(4,9)=1;
  g1(4,16)=params(5)*psi_n_ya__;
  g1(4,23)=(-(params(5)*psi_n_ya__));
  g1(5,21)=1;
  g1(5,10)=1;
  g1(5,11)=(-1);
  g1(6,7)=1;
  g1(6,16)=(-psi_n_ya__);
  g1(7,6)=1;
  g1(7,7)=1;
  g1(7,8)=(-1);
  g1(8,3)=(-params(4));
  g1(8,15)=1;
  g1(8,25)=(-1);
  g1(9,4)=(-params(3));
  g1(9,16)=1;
  g1(9,24)=(-1);
  g1(10,8)=1;
  g1(10,12)=(-(1-params(1)));
  g1(10,16)=(-1);
  g1(11,5)=(-4);
  g1(11,1)=4;
  g1(11,8)=(-4);
  g1(11,2)=(-(4*params(9)));
  g1(11,11)=(-(4*(-params(9))));
  g1(11,14)=1;
  g1(12,8)=(-1);
  g1(12,11)=params(9);
  g1(12,13)=1;
  g1(13,11)=(-4);
  g1(13,18)=1;
  g1(14,10)=(-4);
  g1(14,17)=1;
  g1(15,9)=(-4);
  g1(15,19)=1;
  g1(16,5)=(-4);
  g1(16,20)=1;

if nargout >= 3,
  %
  % Hessian matrix
  %

  g2 = sparse([],[],[],16,625);
if nargout >= 4,
  %
  % Third order derivatives
  %

  g3 = sparse([],[],[],16,15625);
end
end
end
end
