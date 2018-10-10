function [residual, g1, g2, g3] = nkdtc_static(y, x, params)
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

residual = zeros( 9, 1);

%
% Model equations
%

kappa__ = (1-params(6))*(1-params(6)*params(2))*params(8)/(params(6)*(params(8)+params(5)*(1-params(8))))*(1+params(7)+params(8)*(params(1)-1))/params(8);
flex__ = (1+params(7))/(1+params(7)+params(8)*(params(1)-1));
mbar__ = (flex__^params(1)/(1-params(2)))^(1/(1-params(4)));
dbar__ = mbar__^((params(4)-params(11))/(1-params(11)));
zbar__ = mbar__+mbar__^((1-params(4))/(1-params(11)));
T66 = (1-params(4))/mbar__^2;
T72 = params(2)*flex__^(-params(1))*mbar__^(1-params(4));
lhs =y(1);
rhs =y(1)-y(4)-x(5);
residual(1)= lhs-rhs;
lhs =y(2);
rhs =y(2)+(1-params(4))/params(1)*y(3)+y(4)*1/params(1)+x(1);
residual(2)= lhs-rhs;
residual(3) = y(3)*T66+T72*y(5)-(1-params(11))*(y(1)+dbar__*(y(1)-y(3)));
lhs =y(8);
rhs =y(1)*zbar__/(zbar__-mbar__)-y(3)*mbar__/(zbar__-mbar__);
residual(4)= lhs-rhs;
lhs =y(4);
rhs =params(2)*y(4)+kappa__*(y(2)-flex__*y(6))+x(2);
residual(5)= lhs-rhs;
lhs =y(5);
rhs =y(4)*params(3)+y(7);
residual(6)= lhs-rhs;
lhs =y(6);
rhs =(1-params(9))*params(10)+y(6)*params(9)+x(3);
residual(7)= lhs-rhs;
lhs =y(7);
rhs =y(7)*params(12)+x(4);
residual(8)= lhs-rhs;
lhs =y(9);
rhs =y(2)-flex__*y(6);
residual(9)= lhs-rhs;
if ~isreal(residual)
  residual = real(residual)+imag(residual).^2;
end
if nargout >= 2,
  g1 = zeros(9, 9);

  %
  % Jacobian matrix
  %

  g1(1,4)=1;
  g1(2,3)=(-((1-params(4))/params(1)));
  g1(2,4)=(-(1/params(1)));
  g1(3,1)=(-((1-params(11))*(1+dbar__)));
  g1(3,3)=T66-(1-params(11))*(-dbar__);
  g1(3,5)=T72;
  g1(4,1)=(-(zbar__/(zbar__-mbar__)));
  g1(4,3)=mbar__/(zbar__-mbar__);
  g1(4,8)=1;
  g1(5,2)=(-kappa__);
  g1(5,4)=1-params(2);
  g1(5,6)=(-(kappa__*(-flex__)));
  g1(6,4)=(-params(3));
  g1(6,5)=1;
  g1(6,7)=(-1);
  g1(7,6)=1-params(9);
  g1(8,7)=1-params(12);
  g1(9,2)=(-1);
  g1(9,6)=flex__;
  g1(9,9)=1;
  if ~isreal(g1)
    g1 = real(g1)+2*imag(g1);
  end
if nargout >= 3,
  %
  % Hessian matrix
  %

  g2 = sparse([],[],[],9,81);
if nargout >= 4,
  %
  % Third order derivatives
  %

  g3 = sparse([],[],[],9,729);
end
end
end
end
