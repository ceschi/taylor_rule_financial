% run models and load results

dynare nkdtc.mod -Dcalibras=0 -Dz_flag=1 nolog fast nograph;

z_tp = load('nkdtc_results', 'oo_');
z_tp = z_tp.oo_.irfs;

len = load('nkdtc_results', 'options_');
len = len.options_.irf;

dynare nkdtc -Dcalibras=1 -Dz_flag=1 nolog fast nograph noclearall;

z_notp = load('nkdtc_results', 'oo_');
z_notp = z_notp.oo_.irfs;

% plotting section
% dashed lines for TP in place
% dotted lines for no TP
f = figure('visible', 'off');

% y gap
subplot(3, 2, 1);
plot(z_tp.y_gap_e_z, 'black', 'LineStyle', '--', 'LineWidth',1);
hold on;
plot(z_notp.y_gap_e_z, 'black', 'LineStyle', ':', 'LineWidth',1);
line([1 len], [0 0], 'color', 'red');
axis([1 inf -inf inf]);
ylabel('Output gap');
hold off;

% pi
subplot(3, 2, 2);
plot(z_tp.pi_e_z, 'black', 'LineStyle', '--', 'LineWidth',1);
hold on;
plot(z_notp.pi_e_z, 'black', 'LineStyle', ':', 'LineWidth',1);
line([1 len], [0 0], 'color', 'red');
axis([1 inf -inf inf]);
ylabel('Inflation');
hold off;

% s
subplot(3, 2, 3);
plot(z_tp.s_e_z, 'black', 'LineStyle', '--', 'LineWidth',1);
hold on;
plot(z_notp.s_e_z, 'black', 'LineStyle', ':', 'LineWidth',1);
line([1 len], [0 0], 'color', 'red');
axis([1 inf -inf inf]);
ylabel('Policy rate');
hold off;

% m
subplot(3, 2, 4);
plot(z_tp.m_e_z, 'black', 'LineStyle', '--', 'LineWidth',1);
hold on;
plot(z_notp.m_e_z, 'black', 'LineStyle', ':', 'LineWidth',1);
line([1 len], [0 0], 'color', 'red');
axis([1 inf -inf inf]);
ylabel('Money holdings');
hold off;

% z
subplot(3, 2, 5);
plot(z_tp.z_e_z, 'black', 'LineStyle', '--', 'LineWidth',1);
hold on;
plot(z_notp.z_e_z, 'black', 'LineStyle', ':', 'LineWidth',1);
line([1 len], [0 0], 'color', 'red');
axis([1 inf -inf inf]);
ylabel('Real liquidity');
hold off;

% b
subplot(3, 2, 6);
plot(z_tp.b_e_z, 'black', 'LineStyle', '--', 'LineWidth',1);
hold on;
plot(z_notp.b_e_z, 'black', 'LineStyle', ':', 'LineWidth',1);
line([1 len], [0 0], 'color', 'red');
axis([1 inf -inf inf]);
ylabel('Bonds');
hold off;


% saving plot
print(f, 'nkdtc_z_shock', '-deps')%, '-fillpage');

clear f;
clear len;

% % clean up the folder
% delete 'nkdtc.m';
% delete 'nkdtc_static.m';
% delete 'nkdtc_dynamic.m';
% delete 'nkdtc_results.mat';
% delete 'nkdtc_set_auxiliary_variables.m';