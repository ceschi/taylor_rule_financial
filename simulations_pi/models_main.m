%%% Main file for Chapter 1 %%%%
% It runs Gali mod file, saves quartely
% inflation series. It runs nkdtc.mod 
% with two options for the Taylor Rule
% and saves simulated inflations.

clc; clear all;

%%%%% NKDSGE, standard calibration %%%%%%%%
dynare gali_recalib.mod nograph nolog -Dflag_gali=0;

%%%%% other DSGEs %%%%%%%
dynare sw07.mod nograph nolog -Dflag_shock=1;
dynare ascardone14.mod nograph nolog;


%%%%%% Liquidity DSGE, two calibrations: %%%%%%%
% aggressive CB
dynare nkdtc.mod nograph nolog -Dcalibras=0 -Dz_flag=0;
% accommodative CB
dynare nkdtc.mod nograph nolog -Dcalibras=1 -Dz_flag=0;



% add z_flag with only z shock to plot irfs
% or call direclty z_graph
run z_graph.m;


%%%%%% NKDSGE model with multiple calibrations %%%%%%
%%%%%% IRFs

% I - NKDSGE with almost accommodative CB
dynare gali_recalib.mod -Dflag_gali=1 nograph nolog fast;

% II - NKDSGE with insanely aggressive CB
dynare gali_recalib.mod -Dflag_gali=2 nograph nolog fast;