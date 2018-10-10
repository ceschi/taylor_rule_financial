function [residual, g1, g2, g3] = ascardone14_dynamic(y, x, params, steady_state, it_)
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

residual = zeros(20, 1);
T11 = exp(y(7))^params(6);
T23 = exp(y(28))*exp(y(27))^params(6);
T36 = params(17)*exp(y(18))*exp(y(10))^params(4);
T49 = params(5)*exp(y(2))^((1-params(7))*params(15));
T53 = exp(y(9))^(params(7)-1);
T57 = (1-T49*T53)/(1-params(5));
T74 = params(7)/((params(7)-1)*(1-params(3)))*exp(y(13))/exp(y(14));
T80 = exp(y(11))*exp(y(15))^((-1)/(1-params(3)));
T81 = 1/(1-params(3));
T83 = exp(y(7))^(T81-params(6));
T84 = T80*T83;
T90 = params(2)*params(5)*exp(y(9))^(params(7)*(-params(15))/(1-params(3)));
T91 = params(7)/(1-params(3));
T92 = exp(y(28))^T91;
T102 = params(2)*params(5)*exp(y(9))^((1-params(7))*params(15));
T103 = exp(y(28))^(params(7)-1);
T112 = exp(y(7))/exp(y(15));
T114 = exp(y(19))*T112^T81;
T123 = params(5)*exp(y(2))^(params(15)*(-params(7))/(1-params(3)));
T124 = exp(y(9))^T91;
T139 = ((1+exp(y(1)))/(1+params(16)))^params(18);
T143 = (exp(y(9))/params(8))^params(12);
T145 = exp(y(7))/params(14);
T147 = T145^params(13);
T148 = T143*T147;
T150 = T148^(1-params(18));
T162 = exp(y(11))*T81*exp(y(15))^(1/(params(3)-1));
T164 = exp(y(7))^(params(3)/(1-params(3)));
T165 = T162*T164;
T175 = params(17)*exp(y(18))*exp(y(10))^(1+params(4))/(1+params(4));
lhs =1/T11;
rhs =params(2)*(1+exp(y(8)))/T23;
residual(1)= lhs-rhs;
lhs =exp(y(11));
rhs =T11*T36;
residual(2)= lhs-rhs;
lhs =exp(y(12));
rhs =T57^(1/(1-params(7)));
residual(3)= lhs-rhs;
lhs =exp(y(12))^(1+params(7)*params(3)/(1-params(3)));
rhs =T74;
residual(4)= lhs-rhs;
lhs =exp(y(13));
rhs =T84+T90*T92*exp(y(29));
residual(5)= lhs-rhs;
lhs =exp(y(14));
rhs =exp(y(7))^(1-params(6))+T102*T103*exp(y(30));
residual(6)= lhs-rhs;
lhs =exp(y(10));
rhs =T114;
residual(7)= lhs-rhs;
lhs =exp(y(19));
rhs =(1-params(5))*exp(y(12))^((-params(7))/(1-params(3)))+T123*T124*exp(y(5));
residual(8)= lhs-rhs;
lhs =(1+exp(y(8)))/(1+params(16));
rhs =T139*T150*exp(y(20));
residual(9)= lhs-rhs;
lhs =exp(y(16));
rhs =T165;
residual(10)= lhs-rhs;
lhs =exp(y(17));
rhs =(1+exp(y(8)))/exp(y(28));
residual(11)= lhs-rhs;
lhs =y(22);
rhs =y(7)-T175+params(2)*y(31);
residual(12)= lhs-rhs;
lhs =y(20);
rhs =params(9)*y(6)+x(it_, 1);
residual(13)= lhs-rhs;
lhs =y(15);
rhs =params(10)*y(3)+x(it_, 2);
residual(14)= lhs-rhs;
lhs =y(18);
rhs =params(11)*y(4)+x(it_, 3);
residual(15)= lhs-rhs;
lhs =exp(y(21));
rhs =exp(y(15))/exp(y(19));
residual(16)= lhs-rhs;
lhs =exp(y(23));
rhs =1/exp(y(16));
residual(17)= lhs-rhs;
lhs =exp(y(24));
rhs =exp(y(12))/exp(y(16));
residual(18)= lhs-rhs;
lhs =exp(y(25));
rhs =1/exp(y(12));
residual(19)= lhs-rhs;
lhs =exp(y(26));
rhs =T145;
residual(20)= lhs-rhs;
if nargout >= 2,
  g1 = zeros(20, 34);

  %
  % Jacobian matrix
  %

T219 = exp(y(7))*getPowerDeriv(exp(y(7)),params(6),1);
T232 = getPowerDeriv(T112,T81,1);
T239 = getPowerDeriv(T148,1-params(18),1);
T275 = getPowerDeriv(T57,1/(1-params(7)),1);
  g1(1,7)=(-T219)/(T11*T11);
  g1(1,27)=(-((-(params(2)*(1+exp(y(8)))*exp(y(28))*exp(y(27))*getPowerDeriv(exp(y(27)),params(6),1)))/(T23*T23)));
  g1(1,8)=(-(params(2)*exp(y(8))/T23));
  g1(1,28)=(-((-(params(2)*(1+exp(y(8)))*T23))/(T23*T23)));
  g1(2,7)=(-(T36*T219));
  g1(2,10)=(-(T11*params(17)*exp(y(18))*exp(y(10))*getPowerDeriv(exp(y(10)),params(4),1)));
  g1(2,11)=exp(y(11));
  g1(2,18)=(-(T11*T36));
  g1(3,2)=(-((-(T53*params(5)*exp(y(2))*getPowerDeriv(exp(y(2)),(1-params(7))*params(15),1)))/(1-params(5))*T275));
  g1(3,9)=(-(T275*(-(T49*exp(y(9))*getPowerDeriv(exp(y(9)),params(7)-1,1)))/(1-params(5))));
  g1(3,12)=exp(y(12));
  g1(4,12)=exp(y(12))*getPowerDeriv(exp(y(12)),1+params(7)*params(3)/(1-params(3)),1);
  g1(4,13)=(-T74);
  g1(4,14)=(-((-(params(7)/((params(7)-1)*(1-params(3)))*exp(y(13))*exp(y(14))))/(exp(y(14))*exp(y(14)))));
  g1(5,7)=(-(T80*exp(y(7))*getPowerDeriv(exp(y(7)),T81-params(6),1)));
  g1(5,9)=(-(exp(y(29))*T92*params(2)*params(5)*exp(y(9))*getPowerDeriv(exp(y(9)),params(7)*(-params(15))/(1-params(3)),1)));
  g1(5,28)=(-(exp(y(29))*T90*exp(y(28))*getPowerDeriv(exp(y(28)),T91,1)));
  g1(5,11)=(-T84);
  g1(5,13)=exp(y(13));
  g1(5,29)=(-(T90*T92*exp(y(29))));
  g1(5,15)=(-(T83*exp(y(11))*exp(y(15))*getPowerDeriv(exp(y(15)),(-1)/(1-params(3)),1)));
  g1(6,7)=(-(exp(y(7))*getPowerDeriv(exp(y(7)),1-params(6),1)));
  g1(6,9)=(-(exp(y(30))*T103*params(2)*params(5)*exp(y(9))*getPowerDeriv(exp(y(9)),(1-params(7))*params(15),1)));
  g1(6,28)=(-(exp(y(30))*T102*exp(y(28))*getPowerDeriv(exp(y(28)),params(7)-1,1)));
  g1(6,14)=exp(y(14));
  g1(6,30)=(-(T102*T103*exp(y(30))));
  g1(7,7)=(-(exp(y(19))*T112*T232));
  g1(7,10)=exp(y(10));
  g1(7,15)=(-(exp(y(19))*T232*(-(exp(y(7))*exp(y(15))))/(exp(y(15))*exp(y(15)))));
  g1(7,19)=(-T114);
  g1(8,2)=(-(exp(y(5))*T124*params(5)*exp(y(2))*getPowerDeriv(exp(y(2)),params(15)*(-params(7))/(1-params(3)),1)));
  g1(8,9)=(-(exp(y(5))*T123*exp(y(9))*getPowerDeriv(exp(y(9)),T91,1)));
  g1(8,12)=(-((1-params(5))*exp(y(12))*getPowerDeriv(exp(y(12)),(-params(7))/(1-params(3)),1)));
  g1(8,5)=(-(T123*T124*exp(y(5))));
  g1(8,19)=exp(y(19));
  g1(9,7)=(-(exp(y(20))*T139*T143*T145*getPowerDeriv(T145,params(13),1)*T239));
  g1(9,1)=(-(exp(y(20))*T150*exp(y(1))/(1+params(16))*getPowerDeriv((1+exp(y(1)))/(1+params(16)),params(18),1)));
  g1(9,8)=exp(y(8))/(1+params(16));
  g1(9,9)=(-(exp(y(20))*T139*T239*T147*exp(y(9))/params(8)*getPowerDeriv(exp(y(9))/params(8),params(12),1)));
  g1(9,20)=(-(T139*T150*exp(y(20))));
  g1(10,7)=(-(T162*exp(y(7))*getPowerDeriv(exp(y(7)),params(3)/(1-params(3)),1)));
  g1(10,11)=(-T165);
  g1(10,15)=(-(T164*exp(y(11))*T81*exp(y(15))*getPowerDeriv(exp(y(15)),1/(params(3)-1),1)));
  g1(10,16)=exp(y(16));
  g1(11,8)=(-(exp(y(8))/exp(y(28))));
  g1(11,28)=(-((-((1+exp(y(8)))*exp(y(28))))/(exp(y(28))*exp(y(28)))));
  g1(11,17)=exp(y(17));
  g1(12,7)=(-1);
  g1(12,10)=params(17)*exp(y(18))*exp(y(10))*getPowerDeriv(exp(y(10)),1+params(4),1)/(1+params(4));
  g1(12,18)=T175;
  g1(12,22)=1;
  g1(12,31)=(-params(2));
  g1(13,6)=(-params(9));
  g1(13,20)=1;
  g1(13,32)=(-1);
  g1(14,3)=(-params(10));
  g1(14,15)=1;
  g1(14,33)=(-1);
  g1(15,4)=(-params(11));
  g1(15,18)=1;
  g1(15,34)=(-1);
  g1(16,15)=(-(exp(y(15))/exp(y(19))));
  g1(16,19)=(-((-(exp(y(15))*exp(y(19))))/(exp(y(19))*exp(y(19)))));
  g1(16,21)=exp(y(21));
  g1(17,16)=(-((-exp(y(16)))/(exp(y(16))*exp(y(16)))));
  g1(17,23)=exp(y(23));
  g1(18,12)=(-(exp(y(12))/exp(y(16))));
  g1(18,16)=(-((-(exp(y(12))*exp(y(16))))/(exp(y(16))*exp(y(16)))));
  g1(18,24)=exp(y(24));
  g1(19,12)=(-((-exp(y(12)))/(exp(y(12))*exp(y(12)))));
  g1(19,25)=exp(y(25));
  g1(20,7)=(-T145);
  g1(20,26)=exp(y(26));

if nargout >= 3,
  %
  % Hessian matrix
  %

  g2 = sparse([],[],[],20,1156);
if nargout >= 4,
  %
  % Third order derivatives
  %

  g3 = sparse([],[],[],20,39304);
end
end
end
end
