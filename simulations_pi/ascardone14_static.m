function [residual, g1, g2, g3] = ascardone14_static(y, x, params)
%
% Status : Computes static model for Dynare
%
% Inputs : 
%   y         [M_.endo_nbr by 1] double    vector of endogenous variables in declaration order
%   x         [M_.exo_nbr by 1] double     vector of exogenous variables in declaration order
%   params    [M_.param_nbr by 1] double   vector of parameter values in declaration order
%
% Outputs:
%   residual  [M_.endo_nbr by 1] double    vector of residuals of the static model equations 
%                                          in order of declaration of the equations.
%                                          Dynare may prepend or append auxiliary equations, see M_.aux_vars
%   g1        [M_.endo_nbr by M_.endo_nbr] double    Jacobian matrix of the static model equations;
%                                                       columns: variables in declaration order
%                                                       rows: equations in order of declaration
%   g2        [M_.endo_nbr by (M_.endo_nbr)^2] double   Hessian matrix of the static model equations;
%                                                       columns: variables in declaration order
%                                                       rows: equations in order of declaration
%   g3        [M_.endo_nbr by (M_.endo_nbr)^3] double   Third derivatives matrix of the static model equations;
%                                                       columns: variables in declaration order
%                                                       rows: equations in order of declaration
%
%
% Warning : this file is generated automatically by Dynare
%           from model file (.mod)

residual = zeros( 20, 1);

%
% Model equations
%

T11 = exp(y(1))^params(6);
T19 = exp(y(3));
T33 = params(17)*exp(y(12))*exp(y(4))^params(4);
T43 = T19^((1-params(7))*params(15));
T46 = T19^(params(7)-1);
T67 = params(7)/((params(7)-1)*(1-params(3)))*exp(y(7))/exp(y(8));
T73 = exp(y(5))*exp(y(9))^((-1)/(1-params(3)));
T74 = 1/(1-params(3));
T76 = exp(y(1))^(T74-params(6));
T77 = T73*T76;
T83 = params(2)*params(5)*T19^(params(7)*(-params(15))/(1-params(3)));
T85 = T19^(params(7)/(1-params(3)));
T99 = exp(y(1))/exp(y(9));
T101 = exp(y(13))*T99^T74;
T110 = params(5)*T19^(params(15)*(-params(7))/(1-params(3)));
T117 = (1+exp(y(2)))/(1+params(16));
T119 = T117^params(18);
T123 = (T19/params(8))^params(12);
T125 = exp(y(1))/params(14);
T127 = T125^params(13);
T128 = T123*T127;
T130 = T128^(1-params(18));
T142 = exp(y(5))*T74*exp(y(9))^(1/(params(3)-1));
T144 = exp(y(1))^(params(3)/(1-params(3)));
T145 = T142*T144;
T155 = params(17)*exp(y(12))*exp(y(4))^(1+params(4))/(1+params(4));
lhs =1/T11;
rhs =params(2)*(1+exp(y(2)))/(T11*T19);
residual(1)= lhs-rhs;
lhs =exp(y(5));
rhs =T11*T33;
residual(2)= lhs-rhs;
lhs =exp(y(6));
rhs =((1-params(5)*T43*T46)/(1-params(5)))^(1/(1-params(7)));
residual(3)= lhs-rhs;
lhs =exp(y(6))^(1+params(7)*params(3)/(1-params(3)));
rhs =T67;
residual(4)= lhs-rhs;
lhs =exp(y(7));
rhs =T77+exp(y(7))*T83*T85;
residual(5)= lhs-rhs;
lhs =exp(y(8));
rhs =exp(y(1))^(1-params(6))+exp(y(8))*T46*T43*params(2)*params(5);
residual(6)= lhs-rhs;
lhs =exp(y(4));
rhs =T101;
residual(7)= lhs-rhs;
lhs =exp(y(13));
rhs =(1-params(5))*exp(y(6))^((-params(7))/(1-params(3)))+exp(y(13))*T85*T110;
residual(8)= lhs-rhs;
lhs =T117;
rhs =T119*T130*exp(y(14));
residual(9)= lhs-rhs;
lhs =exp(y(10));
rhs =T145;
residual(10)= lhs-rhs;
lhs =exp(y(11));
rhs =(1+exp(y(2)))/T19;
residual(11)= lhs-rhs;
lhs =y(16);
rhs =y(1)-T155+params(2)*y(16);
residual(12)= lhs-rhs;
lhs =y(14);
rhs =y(14)*params(9)+x(1);
residual(13)= lhs-rhs;
lhs =y(9);
rhs =y(9)*params(10)+x(2);
residual(14)= lhs-rhs;
lhs =y(12);
rhs =y(12)*params(11)+x(3);
residual(15)= lhs-rhs;
lhs =exp(y(15));
rhs =exp(y(9))/exp(y(13));
residual(16)= lhs-rhs;
lhs =exp(y(17));
rhs =1/exp(y(10));
residual(17)= lhs-rhs;
lhs =exp(y(18));
rhs =exp(y(6))/exp(y(10));
residual(18)= lhs-rhs;
lhs =exp(y(19));
rhs =1/exp(y(6));
residual(19)= lhs-rhs;
lhs =exp(y(20));
rhs =T125;
residual(20)= lhs-rhs;
if ~isreal(residual)
  residual = real(residual)+imag(residual).^2;
end
if nargout >= 2,
  g1 = zeros(20, 20);

  %
  % Jacobian matrix
  %

T195 = exp(y(1))*getPowerDeriv(exp(y(1)),params(6),1);
T214 = getPowerDeriv(T99,T74,1);
T221 = getPowerDeriv(T128,1-params(18),1);
T247 = T19*getPowerDeriv(T19,(1-params(7))*params(15),1);
T250 = T19*getPowerDeriv(T19,params(7)-1,1);
T263 = T19*getPowerDeriv(T19,params(7)/(1-params(3)),1);
  g1(1,1)=(-T195)/(T11*T11)-(-(params(2)*(1+exp(y(2)))*T19*T195))/(T11*T19*T11*T19);
  g1(1,2)=(-(params(2)*exp(y(2))/(T11*T19)));
  g1(1,3)=(-((-(params(2)*(1+exp(y(2)))*T11*T19))/(T11*T19*T11*T19)));
  g1(2,1)=(-(T33*T195));
  g1(2,4)=(-(T11*params(17)*exp(y(12))*exp(y(4))*getPowerDeriv(exp(y(4)),params(4),1)));
  g1(2,5)=exp(y(5));
  g1(2,12)=(-(T11*T33));
  g1(3,3)=(-((-(T46*params(5)*T247+params(5)*T43*T250))/(1-params(5))*getPowerDeriv((1-params(5)*T43*T46)/(1-params(5)),1/(1-params(7)),1)));
  g1(3,6)=exp(y(6));
  g1(4,6)=exp(y(6))*getPowerDeriv(exp(y(6)),1+params(7)*params(3)/(1-params(3)),1);
  g1(4,7)=(-T67);
  g1(4,8)=(-((-(params(7)/((params(7)-1)*(1-params(3)))*exp(y(7))*exp(y(8))))/(exp(y(8))*exp(y(8)))));
  g1(5,1)=(-(T73*exp(y(1))*getPowerDeriv(exp(y(1)),T74-params(6),1)));
  g1(5,3)=(-(exp(y(7))*(T85*params(2)*params(5)*T19*getPowerDeriv(T19,params(7)*(-params(15))/(1-params(3)),1)+T83*T263)));
  g1(5,5)=(-T77);
  g1(5,7)=exp(y(7))-exp(y(7))*T83*T85;
  g1(5,9)=(-(T76*exp(y(5))*exp(y(9))*getPowerDeriv(exp(y(9)),(-1)/(1-params(3)),1)));
  g1(6,1)=(-(exp(y(1))*getPowerDeriv(exp(y(1)),1-params(6),1)));
  g1(6,3)=(-(exp(y(8))*(T43*params(2)*params(5)*T250+T46*params(2)*params(5)*T247)));
  g1(6,8)=exp(y(8))-exp(y(8))*T46*T43*params(2)*params(5);
  g1(7,1)=(-(exp(y(13))*T99*T214));
  g1(7,4)=exp(y(4));
  g1(7,9)=(-(exp(y(13))*T214*(-(exp(y(1))*exp(y(9))))/(exp(y(9))*exp(y(9)))));
  g1(7,13)=(-T101);
  g1(8,3)=(-(exp(y(13))*(T110*T263+T85*params(5)*T19*getPowerDeriv(T19,params(15)*(-params(7))/(1-params(3)),1))));
  g1(8,6)=(-((1-params(5))*exp(y(6))*getPowerDeriv(exp(y(6)),(-params(7))/(1-params(3)),1)));
  g1(8,13)=exp(y(13))-exp(y(13))*T85*T110;
  g1(9,1)=(-(exp(y(14))*T119*T123*T125*getPowerDeriv(T125,params(13),1)*T221));
  g1(9,2)=exp(y(2))/(1+params(16))-exp(y(14))*T130*exp(y(2))/(1+params(16))*getPowerDeriv(T117,params(18),1);
  g1(9,3)=(-(exp(y(14))*T119*T221*T127*T19/params(8)*getPowerDeriv(T19/params(8),params(12),1)));
  g1(9,14)=(-(T119*T130*exp(y(14))));
  g1(10,1)=(-(T142*exp(y(1))*getPowerDeriv(exp(y(1)),params(3)/(1-params(3)),1)));
  g1(10,5)=(-T145);
  g1(10,9)=(-(T144*exp(y(5))*T74*exp(y(9))*getPowerDeriv(exp(y(9)),1/(params(3)-1),1)));
  g1(10,10)=exp(y(10));
  g1(11,2)=(-(exp(y(2))/T19));
  g1(11,3)=(-((-((1+exp(y(2)))*T19))/(T19*T19)));
  g1(11,11)=exp(y(11));
  g1(12,1)=(-1);
  g1(12,4)=params(17)*exp(y(12))*exp(y(4))*getPowerDeriv(exp(y(4)),1+params(4),1)/(1+params(4));
  g1(12,12)=T155;
  g1(12,16)=1-params(2);
  g1(13,14)=1-params(9);
  g1(14,9)=1-params(10);
  g1(15,12)=1-params(11);
  g1(16,9)=(-(exp(y(9))/exp(y(13))));
  g1(16,13)=(-((-(exp(y(9))*exp(y(13))))/(exp(y(13))*exp(y(13)))));
  g1(16,15)=exp(y(15));
  g1(17,10)=(-((-exp(y(10)))/(exp(y(10))*exp(y(10)))));
  g1(17,17)=exp(y(17));
  g1(18,6)=(-(exp(y(6))/exp(y(10))));
  g1(18,10)=(-((-(exp(y(6))*exp(y(10))))/(exp(y(10))*exp(y(10)))));
  g1(18,18)=exp(y(18));
  g1(19,6)=(-((-exp(y(6)))/(exp(y(6))*exp(y(6)))));
  g1(19,19)=exp(y(19));
  g1(20,1)=(-T125);
  g1(20,20)=exp(y(20));
  if ~isreal(g1)
    g1 = real(g1)+2*imag(g1);
  end
if nargout >= 3,
  %
  % Hessian matrix
  %

  g2 = sparse([],[],[],20,400);
if nargout >= 4,
  %
  % Third order derivatives
  %

  g3 = sparse([],[],[],20,8000);
end
end
end
end
