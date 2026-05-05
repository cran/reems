/*
 * Modified by Felix Weitkämper on 2026-04-11.
 * Adapted as entry point for R interface.
 */

#include "eems.h"
#include "runeems_snps.h"
#include <filesystem>


// The distance metric is a global variable, so that the pairwise_distance function can see it
// Choose 'euclidean' (the default) or 'greatcirc' (great circle distance)
string dist_metric;

void run_eems(Params params)
 {

       try {
    

    params.check_input_arguments();
    
    EEMS eems(params);
    MCMC mcmc(params);
    
    std::filesystem::path dir(eems.prevpath().c_str());
    if (exists(dir)) {
      Rcpp::Rcout << "Load final EEMS state from " << eems.prevpath() << endl << endl;
      eems.load_final_state();
    } else {
      Rcpp::Rcout << "Initialize EEMS random state" << endl << endl;
      eems.initialize_state();
    }
  
    if (!eems.start_eems(mcmc)) {
      Rcpp::stop("[RunEEMS] Error starting EEMS.");
    }
  
    Proposal proposal;

    while (!mcmc.finished) {
      
      switch ( eems.choose_move_type( ) ) {
      case Q_VORONOI_BIRTH_DEATH:
	eems.propose_birthdeath_qVoronoi(proposal);
	break;
      case M_VORONOI_BIRTH_DEATH:
	eems.propose_birthdeath_mVoronoi(proposal);
	break;
      case Q_VORONOI_POINT_MOVE:
	eems.propose_move_one_qtile(proposal);
	break;
      case M_VORONOI_POINT_MOVE:
	eems.propose_move_one_mtile(proposal);
	break;
      case Q_VORONOI_RATE_UPDATE:
	eems.propose_rate_one_qtile(proposal);
	break;
      case M_VORONOI_RATE_UPDATE:
	eems.propose_rate_one_mtile(proposal);
	break;
      case M_MEAN_RATE_UPDATE:
	eems.propose_overall_mrate(proposal);
	break;
      case DF_UPDATE:
	eems.propose_df(proposal,mcmc);
	break;
      default:
	Rcpp::stop("[RunEEMS] Unknown move type\n");
      }
	
      mcmc.add_to_total_moves(proposal.move);
      if (eems.accept_proposal(proposal)) { mcmc.add_to_okay_moves(proposal.move); }
      if (params.testing) { eems.check_ll_computation( ); }
      
      eems.update_sigma2( );
      eems.update_hyperparams( );
      mcmc.end_iteration( );
      
      // Check whether to save the current parameter state,
      // as the thinned out iterations are not saved
      if (mcmc.to_save_iteration()) {
	eems.print_iteration(mcmc);
	eems.save_iteration(mcmc);
      }      
    }
    
    eems.output_results(mcmc);
        
  } catch(exception& e) {
    Rcpp::stop("%s", e.what());
  }    
  
  return;
}

  
  
