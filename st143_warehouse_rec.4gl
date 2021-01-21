#+ WAREHOUSE RECEVING ENQUIRY
#+
#+ Author: Cato Parnell
#+
#+ Enquiry and Logging program to allow viewing of warehouse stock pulling info and logging of progress
#+
#+ CHANGES
#+
#+
{==================================================================================================================================}
IMPORT OS
IMPORT UTIL

& include "sy_lib.inc"

SCHEMA xactdev
        
--Constant variables
    CONSTANT	m_prog_description			= "st143_warehouse_rec",
				m_version_no				= "Ver 1.00",
                m_idle_time                 = 300
                
--Type Definitions
	TYPE		tr_st40_att					RECORD
												loc					STRING,
												whs					STRING,
												doc_type			STRING,
												doc_no				STRING,
												row_id				STRING,
												dest_name			STRING,
												dest_add_1			STRING,
												dest_add_2			STRING,
												dest_add_3			STRING,
												dest_add_4			STRING,
												instructions		STRING,
												collect_req_by		STRING,
												prt_package_labels	STRING,
                                                exported            STRING,
                                                exported_date       STRING,
                                                exported_time       STRING,
												no_of_parcels		STRING,
												priority_lvl		STRING,
												whs_phase_sort		STRING,
												status				STRING,
												start_date			STRING,
												start_time			STRING,
												end_date			STRING,
												end_time			STRING,
												controller			STRING,
												action_by			STRING,
												no_of_items			STRING,
												cut_cbl				STRING,
												trip_sheet			STRING,
												area_code			STRING,
												route_no			STRING,
												del_order			STRING
											END RECORD,
				tr_cust_st40_att			RECORD
												loc					STRING,
												whs					STRING,
												doc_type			STRING,
												doc_no				STRING,
												row_id				STRING,
												dest_name			STRING,
												dest_add_1			STRING,
												dest_add_2			STRING,
												dest_add_3			STRING,
												dest_add_4			STRING,
												instructions		STRING,
												collect_req_by		STRING,
												prt_package_labels	STRING,
                                                exported            STRING,
                                                exported_date       STRING,
                                                exported_time       STRING,
												no_of_parcels		STRING,
												priority_lvl		STRING,
												whs_phase_sort		STRING,
												status				STRING,
												start_date			STRING,
												start_time			STRING,
												end_date			STRING,
												end_time			STRING,
												controller			STRING,
												action_by			STRING,
												no_of_items			STRING,
												cut_cbl				STRING,
												trip_sheet			STRING,
												area_code			STRING,
												route_no			STRING,
												del_order			STRING,
												cust_name			STRING
											END RECORD,
			tr_doc_st40                     RECORD 
												doc_no			LIKE st40_track_store_pulling.doc_no,
												whs				LIKE st40_track_store_pulling.whs,
												doc_type		LIKE st40_track_store_pulling.doc_type,
												customer		LIKE sa25_inv_hd.dl_name,
												del_add_1		LIKE sa25_inv_hd.del_add_1,
												del_add_2		LIKE sa25_inv_hd.del_add_2,
												del_add_3		LIKE sa25_inv_hd.del_add_3,
												del_add_4		LIKE sa25_inv_hd.del_add_4,
												no_of_items		LIKE st40_track_store_pulling.no_of_items,
												cut_cbl			LIKE st40_track_store_pulling.cut_cbl,
                                                trip_sheet      LIKE st40_track_store_pulling.trip_sheet,
												priority_lvl	LIKE st40_track_store_pulling.priority_lvl,
												whs_phase_sort	LIKE st40_track_store_pulling.whs_phase_sort,
                                                status          LIKE st40_track_store_pulling.status,
                                                action_by_user  LIKE st40_track_store_pulling.action_by,
												area_code		LIKE st41_area_mast.area_code,
												area_desc		LIKE st41_area_mast.area_desc,
												route_no		LIKE st41_area_mast.route_no,
												del_order		LIKE st40_track_store_pulling.del_order
											END RECORD
                
				
--Modular variables
	DEFINE		m_prog_type					VARCHAR(3),		--"LOG" = Log an Action
				m_incomplete				STRING,
				m_complete_task				BOOLEAN,

				m_form_title				STRING,
                m_loc_name					STRING,
                m_whs_name                  STRING,
                m_start_date                DATE,
                m_end_date                  DATE,
                m_doc_type                  STRING,
                m_phase                     STRING,
                m_scanner_mode              BOOLEAN,
                m_cr                        INTEGER,
                m_qty_tracking_enabled      BOOLEAN,
				m_doc_no                	LIKE st40_track_store_pulling.doc_no
                
--Modular Record definitions
	DEFINE		mr_st40_action				RECORD
												user_name		LIKE st40u_warehouse_user.user_name,
												full_name		LIKE st40u_warehouse_user.full_name,
												doc_type		LIKE st40_track_store_pulling.doc_type,
												doc_no			LIKE st40_track_store_pulling.doc_no,
                                                ship_doc_no     LIKE ib30_ship_doc_hd.ship_doc_no
											END RECORD,
				mr_st40_org					RECORD LIKE st40_track_store_pulling.*,

				mr_doc_st40					tr_doc_st40
				
--Modular Array definitions
	DEFINE		ma_st40_priority			DYNAMIC ARRAY OF RECORD LIKE st40_track_store_pulling.*,
				ma_st40_priority_att		DYNAMIC ARRAY OF tr_st40_att,


				ma_doc_st40					DYNAMIC ARRAY OF RECORD
												start_date		LIKE st40_track_store_pulling.start_date,
												start_time		LIKE st40_track_store_pulling.start_time,
												end_date		LIKE st40_track_store_pulling.end_date,
												end_time		LIKE st40_track_store_pulling.end_time,
												minutes_taken	STRING,
												action_by		LIKE st40_track_store_pulling.action_by,
												collect_req_by	LIKE st40_track_store_pulling.collect_req_by,
												status_st40		LIKE st40_track_store_pulling.status,
												whs_phase_sort	LIKE st40_track_store_pulling.whs_phase_sort
											END RECORD,
											
				ma_doc_st40_att				DYNAMIC ARRAY OF RECORD
												start_date		STRING,
												start_time		STRING,
												end_date		STRING,
												end_time		STRING,
												minutes_taken	STRING,
												action_by		STRING,
												collect_req_by	STRING,
												status			STRING,
												whs_phase_sort	STRING
											END RECORD,

				ma_st40_priority_cust		DYNAMIC ARRAY OF RECORD
												loc					LIKE st40_track_store_pulling.loc,
												whs					LIKE st40_track_store_pulling.whs,
												doc_type			LIKE st40_track_store_pulling.doc_type,
												doc_no				LIKE st40_track_store_pulling.doc_no,
												row_id				LIKE st40_track_store_pulling.row_id,
												dest_name			LIKE st40_track_store_pulling.dest_name,
												dest_add_1			LIKE st40_track_store_pulling.dest_add_1,
												dest_add_2			LIKE st40_track_store_pulling.dest_add_2,
												dest_add_3			LIKE st40_track_store_pulling.dest_add_3,
												dest_add_4			LIKE st40_track_store_pulling.dest_add_4,
												instructions		LIKE st40_track_store_pulling.instructions,
												collect_req_by		LIKE st40_track_store_pulling.collect_req_by,
												prt_package_labels	LIKE st40_track_store_pulling.prt_package_labels,
                                                exported            LIKE st40_track_store_pulling.exported,
                                                exported_date       LIKE st40_track_store_pulling.exported_date,
                                                exported_time       LIKE st40_track_store_pulling.exported_time,
												no_of_parcels		LIKE st40_track_store_pulling.no_of_parcels,
												priority_lvl		LIKE st40_track_store_pulling.priority_lvl,
												whs_phase_sort		LIKE st40_track_store_pulling.whs_phase_sort,
												status				LIKE st40_track_store_pulling.status,
												start_date			LIKE st40_track_store_pulling.start_date,
												start_time			LIKE st40_track_store_pulling.start_time,
												end_date			LIKE st40_track_store_pulling.end_date,
												end_time			LIKE st40_track_store_pulling.end_time,
												controller			LIKE st40_track_store_pulling.controller,
												action_by			LIKE st40_track_store_pulling.action_by,
												no_of_items			LIKE st40_track_store_pulling.no_of_items,
												cut_cbl				LIKE st40_track_store_pulling.cut_cbl,
												trip_sheet			LIKE st40_track_store_pulling.trip_sheet,
												area_code			LIKE st40_track_store_pulling.area_code,
												route_no			LIKE st40_track_store_pulling.route_no,
												del_order			LIKE st40_track_store_pulling.del_order,
												cust_name			LIKE sa25_inv_hd.dl_name
											END RECORD,
											
				ma_st40_priority_cust_att	DYNAMIC ARRAY OF tr_cust_st40_att
                
MAIN

	DEFINE	l_err_src	STRING

	LET l_err_src = m_prog_description , " > MAIN "
	
	LET g_schema_name		= ARG_VAL(1)
	LET g_user_name			= ARG_VAL(2)
	LET m_incomplete		= ARG_VAL(3)
	LET m_prog_type			= ARG_VAL(5)

    CALL ui.ComboBox.setDefaultInitializer( "sb_bld_cbox_build" )

    IF ( m_incomplete = "recovery" ) THEN 
        LET m_prog_type = "LOG"
    END IF 
    
	CALL lf_prog_init (	{prog_name}     	m_prog_description||" "||m_prog_type,
						{form_name}     	m_prog_description,
						{ver_no}        	m_version_no,
						{child}         	TRUE,
						{test_prog_access}	TRUE,
						{err_src}			l_err_src )


 	CALL sb_module_init( l_err_src )

    -- Make sure the receiving users are setup
	IF NOT ( lf_chk_whs_phase_users_receiving( l_err_src ) ) THEN 
		CALL lf_exit_program( FALSE, l_err_src ) 
	END IF

    CALL sb_recover_incomplete_input( l_err_src )

	CALL main_menu( l_err_src )

END MAIN


{==================================================================================================================================}
{==================================================================================================================================}
--##################################################################################################################################
{
												MAIN MENU FUNCTIONS
} 
--##################################################################################################################################
{==================================================================================================================================}
{==================================================================================================================================}
FUNCTION MAIN_MENU_FUNCTIONS()
END FUNCTION
{==================================================================================================================================}
#+ Changes:
#+
FUNCTION main_menu( p_err_src )

	DEFINE	p_err_src		    STRING,
			l_array_check 	    INTEGER,
            l_cnt               INTEGER,
            l_action_by         LIKE st40_track_store_pulling.action_by,
            l_whs_phase_sort    LIKE st40_track_store_pulling.whs_phase_sort,
            l_whs_status        LIKE st40_track_store_pulling.status

	DEFINE	lr_st40		 	RECORD LIKE st40_track_store_pulling.*

	LET p_err_src = p_err_src , " > main_menu"

	LET w_cur = ui.Window.getCurrent()
	LET f_cur = w_cur.getForm()

    -- Start and end date
    LET m_start_date    = "1" || "-" || MONTH(TODAY) || "-" || YEAR(TODAY)
    LET m_end_date      = DATE(TODAY)

	CASE m_prog_type 

		WHEN "LOG"
			DIALOG ATTRIBUTES( FIELD ORDER FORM, UNBUFFERED )

				DISPLAY ARRAY ma_st40_priority TO priority.*
					BEFORE ROW 
                        IF ( ARR_CURR() <> 0 ) THEN 
                            CALL sb_set_cyan_highlights( ARR_CURR(), p_err_src )
                        END IF 
                         
                    --------------------------------------------------------------------------------------------
                    AFTER ROW
                        LET l_arr_cnt = ARR_CURR()
                        -- This if statment is specificly here for when you use the 'Doc Enq' and compelte a document. the ARR_CURR() is still that document 
                        -- position so it causes errors
                        IF ( l_arr_cnt <= ma_st40_priority.getLength() ) THEN 
                            CALL bld_st40_tasks_attribute( lf_get_field_value( "st40u_warehouse_user", "whs_color", "whs_status = '"|| ma_st40_priority[l_arr_cnt].status ||"' AND loc IN ('00','"||gr_sy02.default_loc||"') AND whs IN ('00','"||gr_sy02.default_whs||"') AND type = 'R'", p_err_src ) || " reverse", ma_st40_priority_att[l_arr_cnt].* ) RETURNING ma_st40_priority_att[l_arr_cnt].*
                        END IF 
                        
                    --------------------------------------------------------------------------------------------
					ON ACTION SELECT 
                        -- You cannot action Cross Dock Hold elements and PO Holding 
                        IF ( lf_row_exists( "st40u_warehouse_user", "type = 'R' AND whs_phase_user = 'Y' AND (po_hold = 'Y' OR cd_hold = 'Y') AND whs_status = '"||ma_st40_priority[ARR_CURR()].status||"'", p_err_src) ) THEN 
                            CALL sy_winmessage( "Invalid Action", "You cannot action this record!", "exclamation" )
                        ELSE 
                            LET l_array_check = ARR_CURR()
                            CALL w_log_action( ma_st40_priority[l_array_check].doc_no, ma_st40_priority[l_array_check].whs_phase_sort, ma_st40_priority[l_array_check].action_by, p_err_src )

                            CALL sb_set_cyan_highlights( l_array_check, p_err_src )
                        END IF 
                        
                    --------------------------------------------------------------------------------------------
                    ON ACTION doc_enq
                        LET l_array_check = ARR_CURR()

						IF ( l_array_check > 0 ) AND ( l_array_check IS NOT NULL ) THEN 
							CALL bld_st40_tasks( NULL,"default_order", p_err_src )
							CALL w_doc_enq( ma_st40_priority[l_array_check].*, p_err_src )
                            IF ( ma_st40_priority.getLength() > 0 ) THEN 
                                CALL fgl_set_arr_curr(1)
                                CALL sb_set_cyan_highlights( 1, p_err_src )
                            END IF 
						END IF 
                        
                        
				END DISPLAY

                -- Input By name for Doc No and User name
				INPUT BY NAME m_doc_no ATTRIBUTES (WITHOUT DEFAULTS)

                    BEFORE INPUT 
						IF ( m_doc_no IS NULL ) THEN
							CALL DIALOG.nextField( "m_doc_no" )
						END IF
                        CALL DIALOG.setActionActive( "find", 				    FALSE )
                        CALL DIALOG.setActionActive( "doc_enq",			        FALSE )
                        
                    AFTER FIELD m_doc_no
                        IF ( m_doc_no IS NOT NULL ) THEN
                            -- Make sure the document exists
							IF ( lf_row_exists( "st40_track_store_pulling", "doc_no[1,11] = '" || m_doc_no || "' AND loc IN ('"||gr_sy02.default_loc||"','00') AND whs IN ('"||gr_sy02.default_whs||"','00')", p_err_src ) ) THEN
                                LET l_whs_status = lf_get_field_value( "st40_track_store_pulling","FIRST 1 status","doc_no[1,11] = '" || m_doc_no || "' AND end_date IS NULL AND end_time IS NULL ORDER BY whs_phase_sort desc", p_err_src )

                                -- You cannot action Cross Dock Hold elements and PO Holding 
                                IF ( lf_row_exists( "st40u_warehouse_user", "type = 'R' AND whs_phase_user = 'Y' AND (po_hold = 'Y' OR cd_hold = 'Y') AND whs_status = '"||l_whs_status||"'", p_err_src) ) THEN 
                                    CALL sy_winmessage( "Invalid Action", "You cannot action this record!", "exclamation" )
                                ELSE 
                                    LET l_action_by       = lf_get_field_value( "st40_track_store_pulling","FIRST 1 action_by","doc_no[1,11] = '" || m_doc_no || "' ORDER BY whs_phase_sort desc", p_err_src )
                                    LET l_whs_phase_sort  = lf_get_field_value( "st40_track_store_pulling","FIRST 1 whs_phase_sort","doc_no[1,11] = '" || m_doc_no || "' AND end_date IS NULL AND end_time IS NULL ORDER BY whs_phase_sort desc", p_err_src )

                                    CALL w_log_action( m_doc_no, l_whs_phase_sort, l_action_by, p_err_src )
                                    LET m_doc_no = NULL
                                    CALL DIALOG.nextField( "m_doc_no" )
                                END IF 
                                
							ELSE
								CALL fgl_winmessage ( %"Invalid Input", "Document Number is not valid", "exclamation" )
								CALL DIALOG.nextField( "m_doc_no" )
							END IF
						ELSE
                            -- When the user is IN or OUT of scanner mode we need to specify where in the dialog the user must go ( Text Edit Field or table ) 
                            IF ( m_scanner_mode ) THEN 
                                CALL DIALOG.nextField( "m_doc_no" )
                            ELSE 
                                CALL DIALOG.nextField( "priority.doc_no" )
                            END IF 
                        END IF

                    ON ACTION field_lookup
                        CALL w_log_action_lookups( NULL, p_err_src )

                END INPUT
                
				BEFORE DIALOG
                --Set Defaults
                    LET m_phase     = "all"
                    LET m_doc_type  = "all"
                    
				--Build the arrays
					CALL bld_and_display_data( NULL,"default_order", p_err_src )
					CALL DIALOG.setArrayAttributes( "priority", ma_st40_priority_att )
                    IF ( ma_st40_priority.getLength() = 0 ) THEN
                        CALL DIALOG.setActionActive( "doc_enq",           FALSE )
                    ELSE 
                        IF ( lf_row_exists( "st40u_warehouse_user","type = 'R' AND whs_phase_user = 'Y' AND po_hold = 'Y' AND whs_status = '"||ma_st40_priority[ARR_CURR()].status||"'", p_err_src) ) THEN 
                            CALL DIALOG.setActionActive( "doc_enq",       FALSE )
                        ELSE 
                            CALL DIALOG.setActionActive( "doc_enq",       TRUE  )
                        END IF 
                    END IF 
                    
                    CALL DIALOG.setFieldActive( "m_doc_no", 			    FALSE 	)
                    CALL DIALOG.setActionActive( "enq_mode",                FALSE   )
                    CALL f_cur.setElementHidden( "enq_mode",                TRUE    )
                    
                    CALL DIALOG.setActionActive( "scanner_mode",            TRUE    )
                    CALL f_cur.setElementHidden( "scanner_mode",            FALSE   )

				AFTER DIALOG
					NEXT FIELD CURRENT

				ON IDLE 30
                    LET l_array_check = ARR_CURR()
					CALL bld_and_display_data( NULL,"default_order", p_err_src )

                    -- When in Scanner mode DO NOT highlight on refresh ( Will cause duplicates ) 
                    IF ( l_array_check > 0 ) AND ( m_scanner_mode = FALSE ) THEN 
                        CALL sb_set_cyan_highlights( l_array_check, p_err_src )
                    END IF 
                    
                -- MODES -------------------------------------------------------------
                ON ACTION enq_mode
                	LET w_cur = ui.Window.getCurrent()
                    LET f_cur = w_cur.getForm()
                    CALL DIALOG.setFieldActive( "m_doc_no", 			    FALSE )
                    CALL DIALOG.setActionActive( "enq_mode",                FALSE )
                    CALL DIALOG.setActionActive( "find", 				    TRUE  )
                    CALL DIALOG.setActionActive( "doc_enq",			        TRUE  )
                    CALL DIALOG.setActionActive( "scanner_mode",            TRUE  )
                    
                    CALL f_cur.setElementHidden( "enq_mode",                TRUE  )
                    CALL f_cur.setElementHidden( "scanner_mode",            FALSE )
                    
                    LET m_scanner_mode   = FALSE  
                    LET m_doc_no    = NULL 
                    CALL ui.Interface.refresh()
                    
                ON ACTION scanner_mode
                	LET w_cur = ui.WINDOW.getCurrent()
                    LET f_cur = w_cur.getForm()
                    -- Note when in scanner mode user has no other functionality -> Find gets disabled
                    CALL DIALOG.setFieldActive( "m_doc_no", 			    TRUE  )
                    CALL DIALOG.setActionActive( "enq_mode",                TRUE  )
                    CALL DIALOG.setActionActive( "scanner_mode",            FALSE )

                    CALL f_cur.setElementHidden( "enq_mode",                FALSE )
                    CALL f_cur.setElementHidden( "scanner_mode",            TRUE  )
                    CALL DIALOG.nextField( "m_doc_no" )

                    LET m_scanner_mode = TRUE 
                    CALL ui.Interface.refresh()
                   
                -- DOCUMENT ACTIONS -------------------------------------------------------------
                ON ACTION po_rec
                    CALL sb_open_receiving_program( ARR_CURR(), "po_rec", p_err_src )  
                    
                ON ACTION grn_a_po
                    CALL sb_open_receiving_program( ARR_CURR(), "grn_a_po", p_err_src )  
                    
                ON ACTION direct_grn
                    CALL sb_open_receiving_program( ARR_CURR(), "direct_grn", p_err_src )  
                    
                ON ACTION linked_c_notes
                    CALL sb_open_receiving_program( ARR_CURR(), "linked_c_notes", p_err_src ) 
                    
                ON ACTION direct_c_notes
                    CALL sb_open_receiving_program( ARR_CURR(), "direct_c_notes", p_err_src )
                    
                ON ACTION receive_ibt
                    CALL sb_open_receiving_program( ARR_CURR(), "receive_ibt", p_err_src )

                    
                -- PHASE ACTIONS -------------------------------------------------------------
				ON ACTION refresh  
                    LET l_array_check = ARR_CURR()
					CALL bld_and_display_data( NULL,"default_order", p_err_src )

                    -- When in Scanner mode DO NOT highlight on refresh ( Will cause duplicates ) 
                    IF ( m_scanner_mode = FALSE ) THEN 
                        CALL sb_set_cyan_highlights( l_array_check, p_err_src )
                    END IF 
                    
				ON ACTION doc_enq
                
                ON ACTION find
                    IF ( bld_prt_type_lookup( "find", p_err_src ) ) THEN 
                        CALL w_log_action_lookups( "doc_find", p_err_src )
                        LET lr_st40.doc_no          = mr_doc_st40.doc_no
                        LET lr_st40.doc_type        = mr_doc_st40.doc_type
                        LET lr_st40.loc		        = lf_get_field_value( "st40_track_store_pulling", "FIRST 1 loc",	        "doc_no = '"||lr_st40.doc_no||"' GROUP BY 1", p_err_src )
                        LET lr_st40.whs			    = lf_get_field_value( "st40_track_store_pulling", "FIRST 1 whs",		    "doc_no = '"||lr_st40.doc_no||"' GROUP BY 1", p_err_src )
                        LET lr_st40.route_no	    = lf_get_field_value( "st40_track_store_pulling", "FIRST 1 route_no",	    "doc_no = '"||lr_st40.doc_no||"' GROUP BY 1", p_err_src )
                        LET lr_st40.no_of_items		= lf_get_field_value( "st40_track_store_pulling", "FIRST 1 no_of_items",	"doc_no = '"||lr_st40.doc_no||"' GROUP BY 1", p_err_src )
                        LET lr_st40.cut_cbl			= lf_get_field_value( "st40_track_store_pulling", "FIRST 1 cut_cbl",		"doc_no = '"||lr_st40.doc_no||"' GROUP BY 1", p_err_src )
                        LET lr_st40.priority_lvl	= lf_get_field_value( "st40_track_store_pulling", "FIRST 1 priority_lvl",	"doc_no = '"||lr_st40.doc_no||"' GROUP BY 1", p_err_src )
                        LET lr_st40.whs_phase_sort	= lf_get_field_value( "st40_track_store_pulling", "FIRST 1 whs_phase_sort",	"doc_no = '"||lr_st40.doc_no||"' GROUP BY 1", p_err_src )
                        LET lr_st40.area_code		= lf_get_field_value( "st40_track_store_pulling", "FIRST 1 area_code",		"doc_no = '"||lr_st40.doc_no||"' GROUP BY 1", p_err_src )
                        IF ( mr_doc_st40.area_code IS NOT NULL ) THEN
                            LET mr_doc_st40.area_desc	= lf_get_field_value( "st41_area_mast", "FIRST 1 area_desc", "whs = '"|| lr_st40.whs ||"' AND area_code = '" || lr_st40.area_code || "'", p_err_src )
                        END IF
                        LET lr_st40.del_order			= lf_get_field_value( "st40_track_store_pulling", "FIRST 1 del_order", "doc_no = '"||lr_st40.doc_no||"' GROUP BY 1", p_err_src )
                        
                    END IF 
                    -- Make sure a doc number is selected before opening the Doc Enq
					CALL bld_and_display_data( NULL,"default_order", p_err_src )

                    IF ( lr_st40.doc_no IS NOT NULL ) THEN 
                        FOR l_cnt = 1 TO ma_st40_priority.getLength()
                            IF ( ma_st40_priority[l_cnt].doc_no = lr_st40.doc_no ) THEN 
                                CALL DIALOG.nextField(	"priority.doc_no" )
                                CALL sb_set_cyan_highlights( l_cnt, p_err_src )
                                EXIT FOR 
                            END IF 
                        END FOR 
                        CALL DIALOG.setCurrentRow( "priority", l_cnt )
                        CALL fgl_set_arr_curr(l_cnt)
                    ELSE 
                        CALL sb_set_cyan_highlights( ARR_CURR(), p_err_src )
                        CALL DIALOG.setCurrentRow( "priority", ARR_CURR() )
                        CALL fgl_set_arr_curr(ARR_CURR())
                    END IF 
                    
                -- FILTER ACTIONS -------------------------------------------------------------
                ON ACTION by_phases
                    LET l_array_check = ARR_CURR()
                    IF ( bld_prt_type_lookup( "by_phases", p_err_src ) ) THEN 
                        -- Set Up Attributes:
                        IF ( m_phase = "all" ) THEN 
                            CALL lf_change_attribute(	{WindowName}	"w_main",
                                                        {ElementType}	"Button",
                                                        {Att_to_chg}	"tag",
                                                        {Att_compare}	"by_phases",
                                                        {att_1_name}	"image",
                                                        {att_1_val}		"filter_clear.png",
                                                        {att_2_name}	"",
                                                        {att_2_val}		"",
                                                        {att_3_name}	"",
                                                        {att_3_val}		"",
                                                        {att_4_name}	"",
                                                        {att_4_val}		"")
                        ELSE 
                            CALL lf_change_attribute(	{WindowName}	"w_main",
                                                        {ElementType}	"Button",
                                                        {Att_to_chg}	"tag",
                                                        {Att_compare}	"by_phases",
                                                        {att_1_name}	"image",
                                                        {att_1_val}		"filter_on.png",
                                                        {att_2_name}	"",
                                                        {att_2_val}		"",
                                                        {att_3_name}	"",
                                                        {att_3_val}		"",
                                                        {att_4_name}	"",
                                                        {att_4_val}		"")
                        END IF 
                        CALL ui.Interface.refresh()

                        CALL bld_and_display_data( "by_phases","default_order", p_err_src )
                    END IF 

                    IF ( l_array_check > 0 ) THEN 
                        CALL sb_set_cyan_highlights( l_array_check, p_err_src )
                    END IF

                    IF ( ma_st40_priority.getLength() = 0 ) THEN
                        CALL DIALOG.setActionActive( "doc_enq",       FALSE )
                    ELSE 
                        CALL DIALOG.setActionActive( "doc_enq",       TRUE  )
                    END IF 
                    
                ON ACTION by_doc_type
                    LET l_array_check = ARR_CURR()
                    IF ( bld_prt_type_lookup( "doc_type", p_err_src ) ) THEN 

                        -- Set Up Attributes:
                        IF ( m_doc_type = "all" ) THEN 
                            CALL lf_change_attribute(	{WindowName}	"w_main",
                                                        {ElementType}	"Button",
                                                        {Att_to_chg}	"tag",
                                                        {Att_compare}	"by_doc_type",
                                                        {att_1_name}	"image",
                                                        {att_1_val}		"filter_clear.png",
                                                        {att_2_name}	"",
                                                        {att_2_val}		"",
                                                        {att_3_name}	"",
                                                        {att_3_val}		"",
                                                        {att_4_name}	"",
                                                        {att_4_val}		"")
                        ELSE 
                            CALL lf_change_attribute(	{WindowName}	"w_main",
                                                        {ElementType}	"Button",
                                                        {Att_to_chg}	"tag",
                                                        {Att_compare}	"by_doc_type",
                                                        {att_1_name}	"image",
                                                        {att_1_val}		"filter_on.png",
                                                        {att_2_name}	"",
                                                        {att_2_val}		"",
                                                        {att_3_name}	"",
                                                        {att_3_val}		"",
                                                        {att_4_name}	"",
                                                        {att_4_val}		"")
                        END IF 
                        
                        CALL bld_and_display_data( "by_doc_type", "default_order", p_err_src )
                    END IF 

                    IF ( l_array_check > 0 ) THEN 
                        CALL sb_set_cyan_highlights( l_array_check, p_err_src )
                    END IF

                    IF ( ma_st40_priority.getLength() = 0 ) THEN
                        CALL DIALOG.setActionActive( "doc_enq",       FALSE )
                    ELSE 
                        CALL DIALOG.setActionActive( "doc_enq",       TRUE  )
                    END IF 
                    
				ON ACTION CLOSE
					EXIT DIALOG

				ON ACTION EXIT
					EXIT DIALOG
			
			END DIALOG
            
	END CASE 

	CALL lf_exit_program( FALSE, p_err_src )

END FUNCTION


{==================================================================================================================================}
{==================================================================================================================================}
--##################################################################################################################################
{
												BUILD AND DISPLAY FUNCTIONS
} 
--##################################################################################################################################
{==================================================================================================================================}
{==================================================================================================================================}
FUNCTION BUILD_AND_DISPLAY_FUNCTIONS()
END FUNCTION
{==================================================================================================================================}
#+ Changes:
#+
FUNCTION bld_and_display_data( p_sort_doc_type, p_sort_order, p_err_src )
 
	DEFINE	p_sort_doc_type     STRING,
            p_sort_order	    STRING, 
			p_err_src		    STRING

	LET p_err_src = p_err_src , " > bld_and_display_data"

	CALL cl_clear_form_and_recs()

	CALL bld_st40_tasks( p_sort_doc_type, p_sort_order, p_err_src )

	CALL display_data_to_forms( p_err_src )

END FUNCTION


{==================================================================================================================================}
{==================================================================================================================================}
{
												END BUILD AND DISPLAY FUNCTIONS
}
{==================================================================================================================================}
{==================================================================================================================================}


{==================================================================================================================================}
{==================================================================================================================================}
--##################################################################################################################################
{
													CLEAR FUNCTIONS
} 
--##################################################################################################################################
{==================================================================================================================================}
{==================================================================================================================================}
FUNCTION CLEAR_FUNCTIONS()
END FUNCTION
{==================================================================================================================================}
#+ CLEAR FORM, RECORDS AND ARRAYS
#+
#+ BUSINESS RULE: 
#+ Clear the form and all modular variables that need to be cleared.
#+
#+ @code CALL cl_clear_form_and_recs()
#+
#+ @param NONE
#+
#+ @return NONE
#+
#+ CHANGES
#+
FUNCTION cl_clear_form_and_recs()

    DEFINE  l_loc_name      STRING,
            l_err_src       STRING
    
	CLEAR FORM

    CALL ma_st40_priority_cust.clear()
    CALL ma_st40_priority_cust_att.clear()

	CALL ma_st40_priority.clear()
	CALL ma_st40_priority_att.clear()

    DISPLAY m_form_title TO title_lbl
	
	CALL ui.interface.refresh()

END FUNCTION


{==================================================================================================================================}
{==================================================================================================================================}
{
													END CLEAR FUNCTIONS
}
{==================================================================================================================================}
{==================================================================================================================================}


{==================================================================================================================================}
{==================================================================================================================================}
--##################################################################################################################################
{
													BUILD FUNCTIONS
} 
--##################################################################################################################################
{==================================================================================================================================}
{==================================================================================================================================}
FUNCTION BUILD_FUNCTIONS()
END FUNCTION
{===================================================================================================================================}
--==================================================================================================================================

#+ Select Type Lookup
#+
#+ BUSINESS RULE: #+
#+ 	Type Lookup
#+
#+ @code    CALL bld_prt_type_lookup ( p_action_selected, p_err_src ) 
#+
#+ @param   p_action_selected       Action Selected
#+ @param   p_err_src               Error Source
#+
#+ CHANGES
#+

FUNCTION bld_prt_type_lookup( p_action_selected, p_err_src )
		
	DEFINE	p_action_selected       STRING,
            p_err_src				STRING,
            
			l_arr_cnt				INTEGER,
			l_cont					BOOLEAN,
            l_type_selected         STRING,
            l_sql                   STRING,
            l_attribute_color       LIKE st40u_warehouse_user.whs_color

    DEFINE  la_doc_types				DYNAMIC ARRAY OF RECORD
                                            tick		    BOOLEAN,
                                            user_name       LIKE st40u_warehouse_user.user_name,
                                            TYPE	        VARCHAR(200),
                                            whs_status      LIKE st40u_warehouse_user.whs_status
                                        END RECORD,

            la_doc_types_att            DYNAMIC ARRAY OF RECORD 
                                            tick		    STRING,
                                            user_name       STRING,
                                            TYPE	        STRING,
                                            whs_status      STRING
                                        END RECORD 

	LET p_err_src = p_err_src || " > bld_prt_type_lookup"
		
    OPEN WINDOW w_prt_report WITH FORM "st143_warehouse_sel_type"
        
	LET l_cont = FALSE 
    LET w_cur = ui.Window.getCurrent()
	LET f_cur = w_cur.getForm()	
    
	CALL la_doc_types.clear()

    CASE p_action_selected
        WHEN "doc_type"
            DISPLAY "SELECT DOCUMENT TYPE" TO warehouse_enq_lbl
            CALL w_cur.setText( "Select Document Type" )
            
            LET la_doc_types[1].type 	= "All - All"
            LET la_doc_types[1].tick 	= 0

            LET la_doc_types[2].type 	= "CRN - Credit Notes"
            LET la_doc_types[2].tick 	= 0

            LET la_doc_types[3].type 	= "IBT - Inter Branch Transfer"
            LET la_doc_types[3].tick 	= 0

            LET la_doc_types[4].type 	= "P/O - Purchase Order Received"
            LET la_doc_types[4].tick 	= 0
            
            LET la_doc_types[5].type 	= "GRN - Goods Recieved Notes"
            LET la_doc_types[5].tick 	= 0

            LET la_doc_types[6].type 	= "BOM - Bill Of Materials"
            LET la_doc_types[6].tick 	= 0
            
			WHILE (1=1)

				IF NOT ( m_doc_type MATCHES "*CRN*" ) THEN
					EXIT WHILE
				END IF

				IF NOT ( m_doc_type MATCHES "*IBT*" ) THEN
					EXIT WHILE
				END IF

				IF NOT ( m_doc_type MATCHES "*P/O*" ) THEN
					EXIT WHILE
				END IF

				IF NOT ( m_doc_type MATCHES "*GRN*" ) THEN
					EXIT WHILE
				END IF

				IF NOT ( m_doc_type MATCHES "*BOM*" ) THEN
					EXIT WHILE
				END IF

				LET m_doc_type = 'all'

			END WHILE
            
            FOR l_arr_cnt = 1 TO la_doc_types.getLength()

            -- if Doc Type is null then set the First record 'All' as the default
                IF ( m_doc_type IS NULL ) OR ( m_doc_type = 'all' ) THEN 
                    LET la_doc_types[1].tick = 1   
                    EXIT FOR 
                END IF 
                
                CASE ( l_arr_cnt )  
                    WHEN 2
                        IF ( m_doc_type MATCHES "*CRN*" ) THEN
                            LET la_doc_types[2].tick = 1
                        END IF  

                    WHEN 3
                        IF ( m_doc_type MATCHES "*IBT*" ) THEN
                            LET la_doc_types[3].tick = 1
                        END IF 
                        
                    WHEN 4
                        IF ( m_doc_type MATCHES "*PO*" ) THEN
                            LET la_doc_types[4].tick = 1
                        END IF 
                        
                    WHEN 5
                        IF ( m_doc_type MATCHES "*GRN*" ) THEN
                            LET la_doc_types[5].tick = 1
                        END IF  

                    WHEN 6
                        IF ( m_doc_type MATCHES "*BOM*" ) THEN
                            LET la_doc_types[6].tick = 1
                        END IF  

                END CASE 
            END FOR

        WHEN "by_phases"
            DISPLAY "SELECT PRIORITY" TO warehouse_enq_lbl
            CALL w_cur.setText( "Select Priority" )
            LET l_sql = "SELECT "                   ||
                            "user_name, "           ||
                            "full_name, "           ||
                            "whs_status "           ||
                        "FROM "                     ||
                            "st40u_warehouse_user " ||
                        "WHERE "                    ||
                            "whs_phase_user = 'Y' " ||"AND "||
                            "type = 'R' "           ||
                        "ORDER BY whs_phase_sort ASC "

            PREPARE qry_whs_users FROM l_sql
            DECLARE curs_whs_users CURSOR FOR qry_whs_users

            LET l_arr_cnt = 2
            LET la_doc_types[1].user_name       = NULL
            LET la_doc_types[1].TYPE            = "All"
            LET la_doc_types[1].whs_status      = "all"
            LET la_doc_types[1].tick            = 0

            FOREACH curs_whs_users INTO la_doc_types[l_arr_cnt].user_name, la_doc_types[l_arr_cnt].TYPE,la_doc_types[l_arr_cnt].whs_status
                LET l_attribute_color = lf_get_field_value( "st40u_warehouse_user", "whs_color","whs_status = '"||la_doc_types[l_arr_cnt].whs_status||"' AND type = 'R'", p_err_src)
                LET la_doc_types_att[l_arr_cnt].TYPE = l_attribute_color|| " reverse"
                LET la_doc_types_att[l_arr_cnt].tick = l_attribute_color|| " reverse"

                LET la_doc_types[l_arr_cnt].tick = 0
                LET l_arr_cnt = l_arr_cnt + 1
            END FOREACH

            CALL la_doc_types.deleteElement(l_arr_cnt)

            FOR l_arr_cnt = 1 TO la_doc_types.getLength()
                -- if Phase is null then set the First record 'All' as the default
                IF ( m_phase IS NULL ) OR ( m_phase = 'all' ) THEN 
                    LET la_doc_types[1].tick = 1   
                    EXIT FOR
                END IF

                -- If the status is the same then 'TICK' it
                IF ( m_phase MATCHES  "*"||la_doc_types[l_arr_cnt].whs_status ||"*") THEN 
                    LET la_doc_types[l_arr_cnt].tick = 1   
                END IF 
                
            END FOR
            
        WHEN "find"
            LET la_doc_types[1].type 	= "CRN - Credit Notes"
            LET la_doc_types[1].tick 	= 0

            LET la_doc_types[2].type 	= "IBT - Inter Branch Transfer"
            LET la_doc_types[2].tick 	= 0
            
            LET la_doc_types[3].type 	= "P/O - Purchase Order Received"
            LET la_doc_types[3].tick 	= 0
            
            LET la_doc_types[4].type 	= "GRN - Goods Recieved Notes"
            LET la_doc_types[4].tick 	= 0

            LET la_doc_types[5].type 	= "BOM - Bill Of Materials"
            LET la_doc_types[5].tick 	= 0
            
    END CASE  

	DIALOG ATTRIBUTES (UNBUFFERED, FIELD ORDER FORM)
		INPUT ARRAY la_doc_types FROM reports.* ATTRIBUTES (WITHOUT DEFAULTS, INSERT ROW = FALSE, DELETE ROW = FALSE, AUTO APPEND = FALSE, APPEND ROW = FALSE)
            BEFORE INPUT 
                CALL DIALOG.setArrayAttributes( "reports", la_doc_types_att )

            ON CHANGE tick 
  				IF ( p_action_selected = "doc_type" ) OR ( p_action_selected = "by_phases" )  THEN
					IF ( ARR_CURR() <> 1 ) AND ( la_doc_types[ARR_CURR()].tick = TRUE ) THEN
						LET la_doc_types[1].tick = FALSE
					END IF

					IF ( ARR_CURR() = 1 ) AND ( la_doc_types[ARR_CURR()].tick = TRUE ) THEN
						FOR l_arr_cnt = 2 TO la_doc_types.getLength()
							LET la_doc_types[l_arr_cnt].tick = FALSE
						END FOR
					END IF

					CALL ui.Interface.refresh()
				END IF          

                IF ( la_doc_types[ARR_CURR()].tick = TRUE  ) THEN 
                    CASE p_action_selected 
                        WHEN "find" 
                            CASE ARR_CURR()   
                                WHEN "1"
                                    LET l_type_selected = "CRN"
                                    EXIT DIALOG 
                                    
                                WHEN "2"
                                    LET l_type_selected = "IBT"
                                    EXIT DIALOG 

                                WHEN "3"
                                    LET l_type_selected = "PO" 
                                    EXIT DIALOG 

                                WHEN "4"
                                    LET l_type_selected = "GRN"
                                    EXIT DIALOG 

                                WHEN "5"
                                    LET l_type_selected = "BOM"
                                    EXIT DIALOG 
     
                            END CASE 

                    END CASE 
                END IF
                
			AFTER INPUT
                ACCEPT DIALOG

        END INPUT
    
		ON ACTION ACCEPT
                CASE ( p_action_selected )
                    WHEN "doc_type"
                        CALL ui.Interface.refresh()
                        LET l_type_selected = NULL
                        
                        FOR l_arr_cnt = 1 TO la_doc_types.getLength()
                            IF ( la_doc_types[l_arr_cnt].tick = TRUE ) THEN
                                CASE l_arr_cnt
                                    WHEN "1"
                                        LET l_type_selected = "all"
                                        EXIT FOR
                                        
                                    WHEN "2"
                                        LET l_type_selected = "'CRN'"
                                        
                                    WHEN "3"
                                        IF ( l_type_selected IS NULL ) THEN
                                            LET l_type_selected = "'IBT'"
                                        ELSE
                                            LET l_type_selected = l_type_selected || ",'IBT'"
                                        END IF
                                        
                                    WHEN "4"
                                        IF ( l_type_selected IS NULL ) THEN
                                            LET l_type_selected = "'PO'"
                                        ELSE
                                            LET l_type_selected = l_type_selected || ",'PO'"
                                        END IF
                                        
                                    WHEN "5"
                                        IF ( l_type_selected IS NULL ) THEN
                                            LET l_type_selected = "'GRN'"
                                        ELSE
                                            LET l_type_selected = l_type_selected || ",'GRN'"
                                        END IF

                                    WHEN "6"
                                        IF ( l_type_selected IS NULL ) THEN
                                            LET l_type_selected = "'BOM'"
                                        ELSE
                                            LET l_type_selected = l_type_selected || ",'BOM'"
                                        END IF
                                        
                                END CASE 
                                
                            END IF
                            
                        END FOR
                    
                    WHEN "by_phases"
                        CALL ui.Interface.refresh()
                        LET l_type_selected = NULL
                        FOR l_arr_cnt = 1 TO la_doc_types.getLength()
                            IF ( la_doc_types[l_arr_cnt].tick = TRUE ) THEN
                                IF ( la_doc_types[l_arr_cnt].TYPE = "All" ) THEN 
                                    LET l_type_selected = la_doc_types[l_arr_cnt].whs_status
                                    EXIT FOR 
                                END IF 
                                IF ( l_type_selected IS NULL ) THEN
                                    LET l_type_selected = "'"||la_doc_types[l_arr_cnt].whs_status||"'"
                                ELSE
                                    LET l_type_selected = l_type_selected ||",'"||la_doc_types[l_arr_cnt].whs_status||"'"
                                END IF
                            END IF
                        END FOR
                        
                END CASE
			
			EXIT DIALOG
							
		ON ACTION CANCEL
            LET l_cont = FALSE
            EXIT DIALOG
				
	END DIALOG

    
	CLOSE WINDOW w_prt_report

    IF ( l_type_selected IS NOT NULL ) THEN     
        CASE p_action_selected 
            WHEN "find" 
                LET mr_doc_st40.doc_type    = l_type_selected

            WHEN "doc_type"
                LET m_doc_type              = l_type_selected
                
            WHEN "by_phases"
                LET m_phase                 = l_type_selected
                
        END CASE  
        
        RETURN TRUE 
    ELSE
        RETURN FALSE 
    END IF 

END FUNCTION

{===================================================================================================================================}
#+ BUILD ALL ACTIVE TASKS
#+
#+ BUSINESS RULE:
#+ Build all Arrays.
#+ 
#+
#+ @code CALL bld_st40_tasks( p_sort_order, p_err_src )
#+
#+ @param p_sort_order 	Sort user choice setting
#+ @param err_src 		Error Source from calling function.
#+
#+ @return NONE
#+ 
#+ CHANGES
#+

FUNCTION bld_st40_tasks( p_sort_doc_type, p_sort_order, p_err_src )

	DEFINE  p_sort_doc_type STRING,
            p_sort_order	STRING, 
			p_err_src		STRING,

			l_doc_sql		STRING,
			l_doc_cnt		INTEGER

	LET p_err_src = p_err_src , " > bld_st40_tasks"

--Build array of threads

    IF ( m_loc_name IS NULL ) AND ( m_whs_name IS NULL )THEN 
        LET l_doc_sql = 	"SELECT "					||
                                    "* "				||
                                "FROM "					||
                                    "st40_track_store_pulling "	||
                            "WHERE "					||
                                "loc"					||" = '"||gr_sy02.default_loc||"'"	||" AND "||
                                "whs"					||" = '"||gr_sy02.default_whs||"'"	||" AND "||
                                "priority_lvl"			||" = "	||"?"						||" AND "
    ELSE
       LET l_doc_sql =	    "SELECT "					||
                                    "* "				||
                                "FROM "					||
                                    "st40_track_store_pulling "	||
                            "WHERE "					||
                                "loc"					||" = '"|| m_loc_name ||"'"	||" AND "||
                                "whs"					||" = '"|| m_whs_name ||"'"	||" AND "||
                                "priority_lvl"			||" = "	||"?"						 ||" AND "
    END IF 

    IF ( m_prog_type = "ENQ" ) THEN 
        LET l_doc_sql     =    l_doc_sql                 ||
                                    "start_date BETWEEN "||
                                                    "MDY("||MONTH(m_start_date)||","||DAY(m_start_date)||","||YEAR(m_start_date)||") "||
                                                "AND "||
                                                    "MDY("||MONTH(m_end_date)||","||DAY(m_end_date)||","||YEAR(m_end_date)||") "||" AND "

    END IF
    
	CASE ( m_prog_type )

		WHEN "LOG"--Log an Action
			LET l_doc_sql =	l_doc_sql				||
                                "end_date"			||" IS NULL "
		OTHERWISE
			LET l_doc_sql =	l_doc_sql				||
                                "end_date"			||" IS NULL "
	
	END CASE

    -- Sort By Priorty
    CASE m_phase
        WHEN "all"
            -- Dont do anything
        OTHERWISE 
            LET l_doc_sql =	l_doc_sql				||"AND "||
                                "status"			||" IN (" ||m_phase||") "
    END CASE  

        -- Sort By Priorty
    CASE m_doc_type
        WHEN "all"
            -- Dont do anything
        OTHERWISE 
            LET l_doc_sql =	l_doc_sql				||"AND "||
                                "doc_type"			||" IN (" ||m_doc_type||") "
    END CASE  

	CASE ( p_sort_order )

		WHEN "route_no_order"
			LET l_doc_sql =	    l_doc_sql					||
								"ORDER BY "					||
										"route_no ASC, "	||
										"del_order, "		||
										"area_code, "		||
										"whs_phase_sort, "	||
										"start_date DESC, "	||
										"start_time DESC"

		WHEN "trip_sheet_no_order"
			LET l_doc_sql =	    l_doc_sql					||
								"ORDER BY "					||
										"trip_sheet DESC, "	||
										"route_no, "		||
										"del_order, "		||
										"area_code, "		||
										"whs_phase_sort, "	||
										"start_date DESC, "	||
										"start_time DESC"

		OTHERWISE
			LET l_doc_sql =	    l_doc_sql					||
								"ORDER BY "					||
										"whs_phase_sort,"	||
                                        "trip_sheet,"       ||
										"start_date DESC, "	||
										"start_time DESC"

	END CASE

	DISPLAY l_doc_sql
	PREPARE qry_tasks FROM l_doc_sql
	DECLARE curs_task CURSOR FOR qry_tasks

------------------------------------------------------------------------------------------------------------------------------------
	LET l_doc_cnt = 1
	FOREACH curs_task USING "5" INTO ma_st40_priority[l_doc_cnt].*

        -- Check to see if the next phase is force assign user true -> if it is and there is active NON phase user enteries into st40 then we clear this st40 entry
        IF ( sb_force_assign_user_true( FALSE, ma_st40_priority[l_doc_cnt].whs_phase_sort, ma_st40_priority[l_doc_cnt].loc, ma_st40_priority[l_doc_cnt].whs, ma_st40_priority[l_doc_cnt].doc_type, ma_st40_priority[l_doc_cnt].doc_no, p_err_src ) ) THEN 
            -- Confirm that the current line is NOT a NON phase user -> if it is a Phase user then remove it
            IF ( lf_row_exists( "st40u_warehouse_user", "whs_phase_user = 'Y' AND user_name = '"||ma_st40_priority[l_doc_cnt].action_by||"' AND type = 'R'", p_err_src ) ) THEN   
                INITIALIZE ma_st40_priority[l_doc_cnt].* TO NULL 
                CONTINUE FOREACH
            END IF 
        END IF  

	--Check if an active task exists, otherwise skip the default "Unassigned" task
		IF ( ma_st40_priority[l_doc_cnt].whs_phase_sort = "0" ) THEN
            IF ( m_loc_name IS NULL ) AND ( m_whs_name IS NULL )THEN  

            ELSE 
                IF ( lf_row_exists(	{tbl_name}		"st40_track_store_pulling",
                                    {where_clause}	"loc"		||" = "	||"'"	|| ma_st40_priority[l_doc_cnt].loc		||"'"		||" AND "||
                                                    "loc"		||" IN" ||"('"	|| m_loc_name						        ||"','00')"	||" AND "||
                                                    "whs"		||" = "	||"'"	|| ma_st40_priority[l_doc_cnt].whs		||"'"		||" AND "||
                                                    "whs"		||" IN" ||"('"	|| m_whs_name       						||"','00')"	||" AND "||
                                                    "doc_type"	||" = "	||"'"	|| ma_st40_priority[l_doc_cnt].doc_type	||"'"		||" AND "||
                                                    "doc_no"	||" = "	||"'"	|| ma_st40_priority[l_doc_cnt].doc_no	||"'"		||" AND "||
                                                    "whs_phase_sort"	||" <> "||"'"|| "0"									||"'",
                                    {err_src}		p_err_src ) )
                THEN
                --Skip Unassigned task
                    CONTINUE FOREACH
                END IF
            END IF 
		END IF
	--Build the attribute array
		CALL bld_st40_tasks_attribute( lf_get_field_value( "st40u_warehouse_user", "whs_color", "whs_status = '"|| ma_st40_priority[l_doc_cnt].status ||"' AND whs_phase_user = 'Y' AND type = 'R'", p_err_src ) || " reverse", ma_st40_priority_att[l_doc_cnt].* ) RETURNING ma_st40_priority_att[l_doc_cnt].*
	--Auto increment array counter
		LET l_doc_cnt = l_doc_cnt + 1
	END FOREACH
	CALL ma_st40_priority.deleteElement( l_doc_cnt )


END FUNCTION


{===================================================================================================================================}
#+ BUILD TASKS - ATTRIBUTES
#+
#+ BUSINESS RULE:
#+ Build attributes for the tasks array.
#+ 
#+
#+ @code CALL bld_st40_tasks_attribute( p_attribute, pr_st40_att )
#+
#+ @param p_attribute Contains the attribute value to assign
#+ @param pr_st40_att Contains the record value of the attribute array
#+
#+ @return NONE
#+ 
#+ CHANGES
#+

FUNCTION bld_st40_tasks_attribute( p_attribute, pr_st40_att )

	DEFINE	p_attribute	STRING

	DEFINE	pr_st40_att	tr_st40_att

	LET pr_st40_att.loc				= p_attribute
	LET pr_st40_att.whs				= p_attribute
	LET pr_st40_att.doc_type		= p_attribute
	LET pr_st40_att.doc_no			= p_attribute
	LET pr_st40_att.dest_name		= p_attribute
	LET pr_st40_att.priority_lvl	= p_attribute
	LET pr_st40_att.whs_phase_sort	= p_attribute
	LET pr_st40_att.status			= p_attribute
	LET pr_st40_att.start_date		= p_attribute
	LET pr_st40_att.start_time		= p_attribute
	LET pr_st40_att.end_date		= p_attribute
	LET pr_st40_att.end_time		= p_attribute
	LET pr_st40_att.controller		= p_attribute
	LET pr_st40_att.action_by		= p_attribute
	LET pr_st40_att.no_of_items		= p_attribute
	LET pr_st40_att.cut_cbl			= p_attribute
	LET pr_st40_att.trip_sheet		= p_attribute
	LET pr_st40_att.area_code		= p_attribute
	LET pr_st40_att.route_no		= p_attribute
	LET pr_st40_att.del_order		= p_attribute

	RETURN pr_st40_att.*

END FUNCTION
 
{===================================================================================================================================}
#+ BUILD DOCUMENT INFORMATION
#+
#+ BUSINESS RULE:
#+ Build record for current document
#+ 
#+
#+ @code CALL bld_doc_enq( pr_st40, p_err_src )
#+
#+ @param pr_st40 Current Task Record
#+ @param p_err_src Error Source from calling function.
#+
#+ @return NONE
#+ 
#+ CHANGES
#+

FUNCTION bld_doc_enq( pr_st40, p_err_src )

	DEFINE	pr_st40			RECORD LIKE st40_track_store_pulling.*,
			p_err_src		STRING,

			l_doc_cnt		INTEGER

	DEFINE	lr_st40			RECORD LIKE st40_track_store_pulling.*,
	
			lr_doc_sa25		RECORD LIKE sa25_inv_hd.*,
			lr_doc_st40		RECORD LIKE st40_track_store_pulling.*

	LET p_err_src = p_err_src , " > bld_doc_enq"

	INITIALIZE mr_doc_st40.* TO NULL

	CASE ( pr_st40.doc_type )
		WHEN "INV"--Invoice
			PREPARE qry_sa25 FROM	"SELECT "					||
											"* "				||
										"FROM "					||
											"sa25_inv_hd "		||
									"WHERE "					||
											"doc_type"	||" = "	|| "'I'"							||" AND "||
											"loc"		||" = "	|| "'"	||gr_sy02.default_loc||"'"	||" AND "||
											"whs"		||" = "	|| "'"	||gr_sy02.default_whs||"'"	||" AND "||
											"inv_type"	||" = "	|| "'I'"||" AND "||
											"doc_no"	||" = "	|| "?"
			EXECUTE qry_sa25 USING pr_st40.doc_no INTO lr_doc_sa25.*

			LET mr_doc_st40.customer	= lr_doc_sa25.dl_name
			LET mr_doc_st40.del_add_1	= lr_doc_sa25.del_add_1
			LET mr_doc_st40.del_add_2	= lr_doc_sa25.del_add_2
			LET mr_doc_st40.del_add_3	= lr_doc_sa25.del_add_3
			LET mr_doc_st40.del_add_4	= lr_doc_sa25.del_add_4

		WHEN "CRN"--Credit Note
			PREPARE qry_sa25_c FROM	"SELECT "					||
											"* "				||
										"FROM "					||
											"sa25_inv_hd "		||
									"WHERE "					||
											"doc_type"	||" = "	|| "'I'"							||" AND "||
											"loc"		||" = "	|| "'"	||gr_sy02.default_loc||"'"	||" AND "||
											"whs"		||" = "	|| "'"	||gr_sy02.default_whs||"'"	||" AND "||
											"inv_type"	||" = "	|| "'C'"||" AND "||
											"doc_no"	||" = "	|| "?"

			EXECUTE qry_sa25_c USING pr_st40.doc_no INTO lr_doc_sa25.*

			LET mr_doc_st40.customer	= lr_doc_sa25.dl_name
			LET mr_doc_st40.del_add_1	= lr_doc_sa25.del_add_1
			LET mr_doc_st40.del_add_2	= lr_doc_sa25.del_add_2
			LET mr_doc_st40.del_add_3	= lr_doc_sa25.del_add_3
			LET mr_doc_st40.del_add_4	= lr_doc_sa25.del_add_4
			
			
		WHEN "IBT"--Inter-Branch Document

			PREPARE qry_st40_ib FROM	"SELECT "								||
												"FIRST 1 * "					||
											"FROM "								||
												"st40_track_store_pulling "		||
										"WHERE "								||
												"doc_no"	||" = "	|| "?" 										||" AND "||
												"loc"		||" IN "|| "('"	||gr_sy02.default_loc||"','00')"	||" AND "||
												"whs"		||" IN "|| "('"	||gr_sy02.default_whs||"','00')"	||
										"ORDER BY "			||
											"row_id DESC"

			EXECUTE qry_st40_ib USING pr_st40.doc_no INTO lr_doc_st40.*

			LET mr_doc_st40.customer	= lr_doc_st40.dest_name
			LET mr_doc_st40.del_add_1	= lr_doc_st40.dest_add_1
			LET mr_doc_st40.del_add_2	= lr_doc_st40.dest_add_2
			LET mr_doc_st40.del_add_3	= lr_doc_st40.dest_add_3
			LET mr_doc_st40.del_add_4	= lr_doc_st40.dest_add_4

		WHEN "IBT"--Inter-Branch Document

			PREPARE qry_st40_ibp FROM	"SELECT "								||
												"FIRST 1 * "					||
											"FROM "								||
												"st40_track_store_pulling "		||
										"WHERE "								||
												"doc_no"	||" = "	|| "?" 										||" AND "||
												"loc"		||" IN "|| "('"	||gr_sy02.default_loc||"','00')"	||" AND "||
												"whs"		||" IN "|| "('"	||gr_sy02.default_whs||"','00')"	||
										"ORDER BY "			||
											"row_id DESC"

			EXECUTE qry_st40_ibp USING pr_st40.doc_no INTO lr_doc_st40.*

			LET mr_doc_st40.customer	= lr_doc_st40.dest_name
			LET mr_doc_st40.del_add_1	= lr_doc_st40.dest_add_1
			LET mr_doc_st40.del_add_2	= lr_doc_st40.dest_add_2
			LET mr_doc_st40.del_add_3	= lr_doc_st40.dest_add_3
			LET mr_doc_st40.del_add_4	= lr_doc_st40.dest_add_4			

		WHEN "COL"--Collection Request
			PREPARE qry_st40 FROM	"SELECT "								||
											"FIRST 1 * "					||
										"FROM "								||
											"st40_track_store_pulling "		||
									"WHERE "								||
											"doc_no"	||" = "	|| "?" 										||" AND "||
											"loc"		||" IN "|| "('"	||gr_sy02.default_loc||"','00')"	||" AND "||
											"whs"		||" IN "|| "('"	||gr_sy02.default_whs||"','00')"	||
									"ORDER BY "			||
										"row_id DESC"

			EXECUTE qry_st40 USING pr_st40.doc_no INTO lr_doc_st40.*

			LET mr_doc_st40.customer	= lr_doc_st40.dest_name
			LET mr_doc_st40.del_add_1	= lr_doc_st40.dest_add_1
			LET mr_doc_st40.del_add_2	= lr_doc_st40.dest_add_2
			LET mr_doc_st40.del_add_3	= lr_doc_st40.dest_add_3
			LET mr_doc_st40.del_add_4	= lr_doc_st40.dest_add_4

		WHEN "PO"--Collection Request
			PREPARE qry_st40_1 FROM	"SELECT "								||
											"FIRST 1 * "					||
										"FROM "								||
											"st40_track_store_pulling "		||
									"WHERE "								||
											"doc_no"	||" = "	|| "?" 										||" AND "||
											"loc"		||" IN "|| "('"	||gr_sy02.default_loc||"','00')"	||" AND "||
											"whs"		||" IN "|| "('"	||gr_sy02.default_whs||"','00')"	||
									"ORDER BY "			||
										"row_id DESC"

			EXECUTE qry_st40_1 USING pr_st40.doc_no INTO lr_doc_st40.*

			LET mr_doc_st40.customer	= lr_doc_st40.dest_name
			LET mr_doc_st40.del_add_1	= lr_doc_st40.dest_add_1
			LET mr_doc_st40.del_add_2	= lr_doc_st40.dest_add_2
			LET mr_doc_st40.del_add_3	= lr_doc_st40.dest_add_3
			LET mr_doc_st40.del_add_4	= lr_doc_st40.dest_add_4

		WHEN "GRV"--Collection Request
			PREPARE qry_st40_2 FROM	"SELECT "								||
											"FIRST 1 * "					||
										"FROM "								||
											"st40_track_store_pulling "		||
									"WHERE "								||
											"doc_no"	||" = "	|| "?" 										||" AND "||
											"loc"		||" IN "|| "('"	||gr_sy02.default_loc||"','00')"	||" AND "||
											"whs"		||" IN "|| "('"	||gr_sy02.default_whs||"','00')"	||
									"ORDER BY "			||
										"row_id DESC"

			EXECUTE qry_st40_2 USING pr_st40.doc_no INTO lr_doc_st40.*

			LET mr_doc_st40.customer	= lr_doc_st40.dest_name
			LET mr_doc_st40.del_add_1	= lr_doc_st40.dest_add_1
			LET mr_doc_st40.del_add_2	= lr_doc_st40.dest_add_2
			LET mr_doc_st40.del_add_3	= lr_doc_st40.dest_add_3
			LET mr_doc_st40.del_add_4	= lr_doc_st40.dest_add_4	

		WHEN "IBR"--Collection Request
			PREPARE qry_st40_3 FROM	"SELECT "								||
											"FIRST 1 * "					||
										"FROM "								||
											"st40_track_store_pulling "		||
									"WHERE "								||
											"doc_no"	||" = "	|| "?" 										||" AND "||
											"loc"		||" IN "|| "('"	||gr_sy02.default_loc||"','00')"	||" AND "||
											"whs"		||" IN "|| "('"	||gr_sy02.default_whs||"','00')"	||
									"ORDER BY "			||
										"row_id DESC"

			EXECUTE qry_st40_3 USING pr_st40.doc_no INTO lr_doc_st40.*

			LET mr_doc_st40.customer	= lr_doc_st40.dest_name
			LET mr_doc_st40.del_add_1	= lr_doc_st40.dest_add_1
			LET mr_doc_st40.del_add_2	= lr_doc_st40.dest_add_2
			LET mr_doc_st40.del_add_3	= lr_doc_st40.dest_add_3
			LET mr_doc_st40.del_add_4	= lr_doc_st40.dest_add_4

		WHEN "GRN"
			PREPARE qry_st40_4 FROM	"SELECT "								||
											"FIRST 1 * "					||
										"FROM "								||
											"st40_track_store_pulling "		||
									"WHERE "								||
											"doc_no"	||" = "	|| "?" 										||" AND "||
											"loc"		||" IN "|| "('"	||gr_sy02.default_loc||"','00')"	||" AND "||
											"whs"		||" IN "|| "('"	||gr_sy02.default_whs||"','00')"	||
									"ORDER BY "			||
										"row_id DESC"

			EXECUTE qry_st40_4 USING pr_st40.doc_no INTO lr_doc_st40.*

			LET mr_doc_st40.customer	= lr_doc_st40.dest_name
			LET mr_doc_st40.del_add_1	= lr_doc_st40.dest_add_1
			LET mr_doc_st40.del_add_2	= lr_doc_st40.dest_add_2
			LET mr_doc_st40.del_add_3	= lr_doc_st40.dest_add_3
			LET mr_doc_st40.del_add_4	= lr_doc_st40.dest_add_4	

		WHEN "BOM"
			PREPARE qry_st40_5 FROM	"SELECT "								||
											"FIRST 1 * "					||
										"FROM "								||
											"st40_track_store_pulling "		||
									"WHERE "								||
											"doc_no"	||" = "	|| "?" 										||" AND "||
											"loc"		||" IN "|| "('"	||gr_sy02.default_loc||"','00')"	||" AND "||
											"whs"		||" IN "|| "('"	||gr_sy02.default_whs||"','00')"	||
									"ORDER BY "			||
										"row_id DESC"

			EXECUTE qry_st40_5 USING pr_st40.doc_no INTO lr_doc_st40.*

			LET mr_doc_st40.customer	= lr_doc_st40.dest_name
			LET mr_doc_st40.del_add_1	= lr_doc_st40.dest_add_1
			LET mr_doc_st40.del_add_2	= lr_doc_st40.dest_add_2
			LET mr_doc_st40.del_add_3	= lr_doc_st40.dest_add_3
			LET mr_doc_st40.del_add_4	= lr_doc_st40.dest_add_4			
			
		OTHERWISE
			CALL fgl_winmessage( %"Invalid Data", %"An invalid row has been selected!\nPlease try again.", "exclamation" )
			RETURN FALSE
			
	END CASE

--General Task data
	LET mr_doc_st40.doc_no			= pr_st40.doc_no
	LET mr_doc_st40.whs				= pr_st40.whs
	LET mr_doc_st40.doc_type		= pr_st40.doc_type
	LET mr_doc_st40.no_of_items		= pr_st40.no_of_items
	LET mr_doc_st40.cut_cbl			= pr_st40.cut_cbl
	LET mr_doc_st40.priority_lvl	= pr_st40.priority_lvl
	LET mr_doc_st40.whs_phase_sort	= pr_st40.whs_phase_sort
	LET mr_doc_st40.area_code		= pr_st40.area_code
    LET mr_doc_st40.route_no        = pr_st40.route_no
    LET mr_doc_st40.status          = pr_st40.status
    LET mr_doc_st40.trip_sheet      = pr_st40.trip_sheet

    IF ( mr_doc_st40.area_code IS NOT NULL ) THEN
		LET mr_doc_st40.area_desc	= lf_get_field_value(	{tbl_name}		"st41_area_mast",
															{fieldname}		"area_desc",
															{where_clause}	"loc = '" 			|| mr_doc_st40.doc_no[1,3] 	|| "' AND "||
																			"whs = '" 			|| mr_doc_st40.whs 		 	|| "' AND "||
																			"area_code = '" 	|| mr_doc_st40.area_code 	|| "'",
															{err_src}		p_err_src )
	END IF
	LET mr_doc_st40.del_order		= pr_st40.del_order

--Build array of threads
	PREPARE qry_doc FROM	"SELECT "			||
									"* "		||
								"FROM "			||
									"st40_track_store_pulling "		||
							"WHERE "			||
									"doc_type"	||" = "	|| "?"											||" AND "||
									"loc"		||" IN "	|| "('"	||gr_sy02.default_loc||"','00')"	||" AND "||
									"whs"		||" IN "	|| "('"	||gr_sy02.default_whs||"','00')"	||" AND "||
									"doc_no"	||" = "	|| "?"		||" "		||
							"ORDER BY "			||
									"row_id"
                                    
	DECLARE curs_doc CURSOR FOR qry_doc

	LET l_doc_cnt = 1
	CALL ma_doc_st40.clear()
	
	FOREACH curs_doc USING pr_st40.doc_type, pr_st40.doc_no INTO lr_st40.*
	--Populate the modular document display array
		LET ma_doc_st40[l_doc_cnt].start_date		= lr_st40.start_date
		LET ma_doc_st40[l_doc_cnt].start_time		= lr_st40.start_time
		LET ma_doc_st40[l_doc_cnt].end_date		= lr_st40.end_date
		LET ma_doc_st40[l_doc_cnt].end_time		= lr_st40.end_time
		LET ma_doc_st40[l_doc_cnt].minutes_taken	= lr_st40.end_time - lr_st40.start_time
		LET ma_doc_st40[l_doc_cnt].action_by		= lr_st40.action_by
		LET ma_doc_st40[l_doc_cnt].collect_req_by	= lr_st40.collect_req_by
		LET ma_doc_st40[l_doc_cnt].status_st40 	= lr_st40.status
		LET ma_doc_st40[l_doc_cnt].whs_phase_sort	= lr_st40.whs_phase_sort
		
	--Build the attribute array
		CALL bld_doc_enq_attribute( lf_get_field_value( "st40u_warehouse_user", "whs_color", "whs_status = '"|| ma_doc_st40[l_doc_cnt].status_st40 ||"' AND loc IN ('00','"||gr_sy02.default_loc||"') AND whs IN ('00','"||gr_sy02.default_whs||"')", p_err_src ) || " reverse", l_doc_cnt )
		
	--Auto increment array counter
		LET l_doc_cnt = l_doc_cnt + 1
	END FOREACH
    
	CALL ma_doc_st40.deleteElement( l_doc_cnt )

	RETURN TRUE

END FUNCTION


{===================================================================================================================================}
#+ BUILD DOCUMENT TASKS - ATTRIBUTES
#+
#+ BUSINESS RULE:
#+ Build attributes for the document tasks array.
#+ 
#+ @code CALL bld_doc_enq_attribute( p_attribute, p_task_cnt )
#+
#+ @param p_attribute Contains the attribute value to assign
#+ @param p_task_cnt Current row of array
#+
#+ @return NONE
#+ 
#+ CHANGES
#+
FUNCTION bld_doc_enq_attribute( p_attribute, p_task_cnt )

	DEFINE	p_attribute	STRING,
			p_task_cnt	INTEGER

	LET ma_doc_st40_att[p_task_cnt].start_date		= p_attribute
	LET ma_doc_st40_att[p_task_cnt].start_time		= p_attribute
	LET ma_doc_st40_att[p_task_cnt].end_date		= p_attribute
	LET ma_doc_st40_att[p_task_cnt].end_time		= p_attribute
	LET ma_doc_st40_att[p_task_cnt].minutes_taken	= p_attribute
	LET ma_doc_st40_att[p_task_cnt].action_by		= p_attribute
	LET ma_doc_st40_att[p_task_cnt].collect_req_by	= p_attribute
	LET ma_doc_st40_att[p_task_cnt].status			= p_attribute
	LET ma_doc_st40_att[p_task_cnt].whs_phase_sort	= p_attribute

END FUNCTION

{===================================================================================================================================}
#+ BUILD TASKS - ATTRIBUTES
#+
#+ BUSINESS RULE:
#+ Build attributes for the tasks array.
#+ 
#+
#+ @code CALL bld_cust_st40_tasks_attribute( p_attribute, pr_st40_att )
#+
#+ @param p_attribute Contains the attribute value to assign
#+ @param pr_st40_att Contains the record value of the attribute array
#+
#+ @return NONE
#+ 
#+ CHANGES
#+
FUNCTION bld_cust_st40_tasks_attribute( p_attribute, pr_st40_att )

	DEFINE	p_attribute	STRING

	DEFINE	pr_st40_att	tr_cust_st40_att

	LET pr_st40_att.loc				= p_attribute
	LET pr_st40_att.whs				= p_attribute
	LET pr_st40_att.doc_type		= p_attribute
	LET pr_st40_att.doc_no			= p_attribute
	LET pr_st40_att.dest_name		= p_attribute
	LET pr_st40_att.priority_lvl	= p_attribute
	LET pr_st40_att.whs_phase_sort	= p_attribute
	LET pr_st40_att.status			= p_attribute
	LET pr_st40_att.dest_name		= p_attribute
	LET pr_st40_att.start_date		= p_attribute
	LET pr_st40_att.start_time		= p_attribute
	LET pr_st40_att.end_date		= p_attribute
	LET pr_st40_att.end_time		= p_attribute
	LET pr_st40_att.controller		= p_attribute
	LET pr_st40_att.action_by		= p_attribute
	LET pr_st40_att.no_of_items		= p_attribute
	LET pr_st40_att.cut_cbl			= p_attribute
	LET pr_st40_att.trip_sheet		= p_attribute
	LET pr_st40_att.area_code		= p_attribute
	LET pr_st40_att.route_no		= p_attribute
	LET pr_st40_att.del_order		= p_attribute
	LET pr_st40_att.cust_name		= p_attribute

	RETURN pr_st40_att.*

END FUNCTION

{==================================================================================================================================}
{==================================================================================================================================}
{
													END BUILD FUNCTIONS
}
{==================================================================================================================================}
{==================================================================================================================================}


{==================================================================================================================================}
{==================================================================================================================================}
--##################################################################################################################################
{
													DISPLAY FUNCTIONS
} 
--##################################################################################################################################
{==================================================================================================================================}
{==================================================================================================================================}
FUNCTION DISPLAY_FUNCTIONS()
END FUNCTION
{==================================================================================================================================}
#+ DISPLAY RECORD AND ARRAY
#+
#+ BUSINESS RULE:
#+ Display all data to the form.
#+
#+ @code CALL display_data_to_forms( p_err_src )
#+
#+ @param p_err_src Error Source from calling statement
#+
#+ @return NONE
#+
#+ CHANGES
#+

FUNCTION display_data_to_forms( p_err_src )
	
	DEFINE	p_err_src	STRING
		
	LET p_err_src = p_err_src , " > display_data_to_forms"

    DISPLAY ARRAY ma_st40_priority TO priority.*
        BEFORE DISPLAY
            CALL DIALOG.setArrayAttributes( "priority", ma_st40_priority_att )
            EXIT DISPLAY
    END DISPLAY

END FUNCTION


{==================================================================================================================================}
{==================================================================================================================================}
{
													END DISPLAY FUNCTIONS
}
{==================================================================================================================================}
{==================================================================================================================================}





{==================================================================================================================================}
{==================================================================================================================================}
--##################################################################################################################################
{
													ENQUIRY FUNCTIONS
} 
--##################################################################################################################################
{==================================================================================================================================}
{==================================================================================================================================}
FUNCTION ENQUIRY_FUNCTIONS()
END FUNCTION
{==================================================================================================================================}
#+ DOCUMENT ENQUIRY
#+
#+ BUSINESS RULE:
#+ Display information about the current document
#+
#+ @code CALL w_doc_enq( pr_st40, p_err_src )
#+
#+ @param pr_st40 Current Task Record
#+ @param p_err_src Error Source from calling statement
#+
#+ @return NONE
#+
#+ CHANGES
#+
FUNCTION w_doc_enq( pr_st40, p_err_src )

	DEFINE	pr_st40				RECORD LIKE st40_track_store_pulling.*,
			p_err_src			STRING,

            l_arr_curr      	INTEGER,
			lb_area_code		LIKE st41_area_mast.area_code,
			lb_route_no			LIKE st41_area_mast.route_no,
			l_create_by			LIKE sa25_inv_hd.create_by,
			l_print_phase		LIKE st40_track_store_pulling.whs_phase_sort,
			l_not_printed		STRING,
			l_printed_date		LIKE st40_track_store_pulling.end_date,
			l_printed_time		LIKE st40_track_store_pulling.end_time,
			l_check				BOOLEAN,
            l_close_view        BOOLEAN,
			l_consolidate		BOOLEAN

	LET p_err_src = p_err_src , " > w_doc_enq"

	LET l_consolidate = FALSE
	IF NOT( bld_doc_enq( pr_st40.*, p_err_src ) ) THEN
		RETURN
	END IF

   -- to allow to build customer details once form is closed 
   LET l_arr_curr = ARR_CURR() 

   --Backup the area code
	LET lb_area_code = mr_doc_st40.area_code
	LET lb_route_no	 = mr_doc_st40.route_no

	CALL lf_load_styles( p_err_src )
	OPEN WINDOW w_doc_enq WITH FORM "st143_warehouse_doc_enq"

	DISPLAY BY NAME mr_doc_st40.*

	CASE mr_doc_st40.doc_type
	
	WHEN "CRN"
	
		LET l_create_by = lf_get_field_value( "sa25_inv_hd", "create_by", "doc_no = '"||mr_doc_st40.doc_no||"' ", p_err_src )

	WHEN "IBT"

		LET l_create_by = lf_get_field_value( "ib24_ib_hd", "ibt_user", "ibt_no = '"||mr_doc_st40.doc_no||"' ", p_err_src)

	WHEN "PO"

		LET l_create_by = lf_get_field_value( "pu22_po_hd", "create_by", "doc_no = '"||mr_doc_st40.doc_no||"' ", p_err_src)

	WHEN "GRN"

		LET l_create_by = lf_get_field_value( "pu25_grn_hd", "create_by", "doc_no = '"||mr_doc_st40.doc_no||"' ", p_err_src)		

	WHEN "IBR"

		LET l_create_by = lf_get_field_value( "ib20_req_hd", "req_user", "req_no = '"||mr_doc_st40.doc_no||"' ", p_err_src)
	
	WHEN "IBT"

		LET l_create_by = lf_get_field_value( "ib30_ship_doc_hd", "rec_user", "ibt_no = '"||mr_doc_st40.doc_no[1,11]||"'", p_err_src)

	WHEN "BOM"

		LET l_create_by = lf_get_field_value( "sy20_batch_log sy20, bm20_rec_bt bm20", "sy20.create_by", "sy20.batch_no = bm20.batch_no AND sy20.source = 'WO' AND bm20.wo_no = '"||mr_doc_st40.doc_no||"'", p_err_src)		

	OTHERWISE

		LET l_create_by = " "
	
	END CASE
	
	DISPLAY l_create_by TO create_by

--Get the print package phase
	LET l_print_phase = lf_get_field_value( 	{tbl_name}		"st40u_warehouse_user",
												{field_name}	"whs_phase_sort",
												{where_clause}	"allow_label_prt = 'Y'",
												{err_src}		p_err_src	)

	LET l_not_printed = "Not printed"

	IF( lf_row_exists( "st40_track_store_pulling", "doc_no = '"|| mr_doc_st40.doc_no ||"' AND whs_phase_sort = '"|| l_print_phase ||"' AND end_date IS NOT NULL ", p_err_src ) AND ( l_print_phase IS NOT NULL) ) THEN
	
		LET l_printed_date = lf_get_field_value( 	{tbl_name}		"st40_track_store_pulling",
													{field_name}	"FIRST 1 end_date",
													{where_clause}	"doc_no = '"		||mr_doc_st40.doc_no||"' AND " ||
																	"end_date IS NOT NULL"					||" AND "  ||
																	"whs_phase_sort = " ||l_print_phase,
													{err_src}		p_err_src	)

		LET l_printed_time = lf_get_field_value( 	{tbl_name}		"st40_track_store_pulling",
													{field_name}	"FIRST 1 end_time",
													{where_clause}	"doc_no = '"		||mr_doc_st40.doc_no|| "' AND " ||
																	"end_time IS NOT NULL"					||" AND "   ||
																	"whs_phase_sort = " ||l_print_phase,
													{err_src}		p_err_src	)	

		DISPLAY	l_printed_date TO printed_date		
		DISPLAY	l_printed_time TO printed_time
	ELSE
		DISPLAY l_not_printed TO printed_date
	END IF 	

	IF ( l_create_by IS NOT NULL ) THEN 
		DISPLAY lf_get_field_value( "sy02_user", "full_name", "user_name = '"||l_create_by||"'", p_err_src) TO full_name
	END IF 
	
	DIALOG ATTRIBUTES( FIELD ORDER FORM, UNBUFFERED )
	-----------------------------------------------------------------
		DISPLAY ARRAY ma_doc_st40 TO tbl_docs.*
			BEFORE DISPLAY
				CALL DIALOG.setArrayAttributes( "tbl_docs", ma_doc_st40_att )
		END DISPLAY
	-----------------------------------------------------------------
		INPUT BY NAME mr_doc_st40.* ATTRIBUTES( WITHOUT DEFAULTS )
        
        -------------------------------------------------------------
            ON CHANGE status
                CALL w_doc_is_field_valid( pr_st40.*, "status", p_err_src ) RETURNING g_not_used
                
                -- If status has changed and the current field is Force assign user ( the changed to field )
                IF ( pr_st40.status <> mr_doc_st40.status ) AND ( lf_row_exists( "st40u_warehouse_user", "whs_phase_user = 'Y' AND type = 'R' AND force_assign_user = 'Y' AND priority_level LIKE '%"||mr_doc_st40.priority_lvl||"%' AND whs_status = '"||mr_doc_st40.status||"'", p_err_src ) ) THEN 
                    CALL DIALOG.setFieldActive( "action_by_user",   TRUE )
                    CALL DIALOG.nextfield("action_by_user")
                ELSE 
                    CALL DIALOG.setFieldActive( "action_by_user",   FALSE )
                    LET mr_doc_st40.action_by_user = NULL 
                END IF 
                
        -------------------------------------------------------------
            AFTER FIELD status
                CALL w_doc_is_field_valid( pr_st40.*, "status", p_err_src ) RETURNING g_not_used
                
        -------------------------------------------------------------
            ON CHANGE action_by_user
                CALL w_doc_is_field_valid( pr_st40.*, "action_by_user", p_err_src ) RETURNING g_not_used   
                
        -------------------------------------------------------------
            AFTER FIELD action_by_user
                CALL w_doc_is_field_valid( pr_st40.*, "action_by_user", p_err_src ) RETURNING g_not_used 
                
		-------------------------------------------------------------
			ON CHANGE priority_lvl
				IF ( l_check <> FALSE ) THEN
					CALL upd_doc_enq( mr_doc_st40.*, p_err_src )
				END IF
                
                -- Rebuild status
                CALL sb_cbox_build_doc_enq()
                
                -- Check to see if the current phase exsits for the selected priority.
                IF NOT( lf_row_exists ( "st40u_warehouse_user", "priority_level LIKE '%"||mr_doc_st40.priority_lvl||"%' AND whs_status = '"||mr_doc_st40.status||"'", p_err_src ) ) THEN 
                    LET mr_doc_st40.status = NULL 
                    CALL DIALOG.nextField("status")
                END IF 
                
        -------------------------------------------------------------
			ON CHANGE area_code
            --Update the area code
                CALL upd_doc_enq( mr_doc_st40.*, p_err_src )

        -------------------------------------------------------------
			ON CHANGE route_no
			--Update the route no 
				CALL upd_doc_enq( mr_doc_st40.*, p_err_src )

        -------------------------------------------------------------
			ON CHANGE del_order
			--Update the area code
				CALL upd_doc_enq( mr_doc_st40.*, p_err_src )
                
        -----------------------------------------------------------------
		END INPUT

	-----------------------------------------------------------------
		BEFORE DIALOG
            CALL sb_cbox_build_doc_enq()

			CALL DIALOG.setArrayAttributes( "tbl_docs", ma_doc_st40_att )
		
			CALL DIALOG.setFieldActive( "doc_no",			FALSE )
            CALL DIALOG.setFieldActive( "action_by_user",   FALSE )
			CALL DIALOG.setFieldActive( "doc_type",			FALSE )
			CALL DIALOG.setFieldActive( "customer",			FALSE )
			CALL DIALOG.setFieldActive( "del_add_1",		FALSE )
			CALL DIALOG.setFieldActive( "del_add_2",		FALSE )
			CALL DIALOG.setFieldActive( "del_add_3",		FALSE )
			CALL DIALOG.setFieldActive( "del_add_4",		FALSE )
			CALL DIALOG.setFieldActive( "no_of_items",		FALSE )
			CALL DIALOG.setFieldActive( "cut_cbl",			FALSE )

            --========================== CHECK FOR SERIAL ALLOC ==========================--
            -- Make sure the current PHASE has ibt_con
            IF ( lf_row_exists( "st40u_warehouse_user", "whs_phase_user = 'Y' AND chg_put_bin = 'Y' AND whs_status = '"||pr_st40.status||"' AND type = 'R'", p_err_src ) ) THEN 
                CALL DIALOG.setActionActive( "bin_allocation", TRUE  )
                CALL DIALOG.setActionHidden( "bin_allocation", FALSE )
            ELSE 
                CALL DIALOG.setActionActive( "bin_allocation", FALSE )
                CALL DIALOG.setActionHidden( "bin_allocation", TRUE )   
            END IF 
            --===========================================================================--

			IF ( ( m_prog_type <> "TRA" ) OR ( pr_st40.priority_lvl <> 4 ) ) THEN
				CALL DIALOG.setFieldActive( "area_code",	FALSE )
				CALL DIALOG.setFieldActive( "del_order",	FALSE )
				CALL DIALOG.setFieldActive( "route_no", 	FALSE )
			END IF

            IF ( mr_doc_st40.area_code IS NULL ) THEN 
                CALL DIALOG.setFieldActive( "del_order",	FALSE )
				CALL DIALOG.setFieldActive( "route_no", 	FALSE )
            END IF 
            
            CALL DIALOG.setFieldActive( "area_desc",		FALSE )

			IF ( m_prog_type = "ENQ" ) THEN
				CALL DIALOG.setFieldActive( "priority_lvl",	FALSE )
                CALL DIALOG.setFieldActive( "status",	    FALSE )
				CALL DIALOG.setFieldActive( "area_code",	FALSE )
				CALL DIALOG.setFieldActive( "del_order",	FALSE )
			END IF

	-----------------------------------------------------------------
		AFTER DIALOG
			NEXT FIELD CURRENT

	-----------------------------------------------------------------
        ON ACTION field_lookup 
            LET mr_doc_st40.action_by_user  = lf_lookup(	{tbl_name}          "st40u_warehouse_user",
                                                            {cols}              "loc, whs, user_name, full_name",
                                                            {col_titles}        "LOC, WHS, Username, Full Name ",
                                                            {fld_att}           "V3,V3,V10,V25",
                                                            {flds_to_search}    "user_name",
                                                            {flds_to_return}	"3",
                                                            {input_string}      mr_doc_st40.action_by_user,
                                                            {sub_qry}           "loc IN ('"|| gr_sy02.default_loc ||"','00') AND whs IN ('"|| gr_sy02.default_whs ||"','00') AND ( type = 'R' OR type = 'B' ) AND whs_phase_user = 'N'",
                                                            {ord_by}            "loc,whs_phase_sort,user_name",
                                                            {win_title}         "Warehouse User Lookup",
                                                            {err_src}			p_err_src)

    -----------------------------------------------------------------
        ON ACTION bin_allocation
            CALL lf_bin_warehouse_allocation( "R", pr_st40.doc_no, pr_st40.doc_type, gr_sy02.default_loc, gr_sy02.default_whs, p_err_src ) RETURNING g_not_used

	-----------------------------------------------------------------
		ON ACTION close_view
            -- Make sure status is NOT null
            CALL w_doc_is_field_valid( pr_st40.*, "status", p_err_src ) RETURNING l_close_view

            IF ( l_close_view = FALSE ) THEN 
                CONTINUE DIALOG 
            END IF 
            
            IF ( pr_st40.status <> mr_doc_st40.status ) THEN 
                CALL upd_status( pr_st40.*, mr_doc_st40.*, p_err_src )
            END IF
            EXIT DIALOG

	-----------------------------------------------------------------
		ON ACTION CLOSE
            -- Make sure status is NOT null
            CALL w_doc_is_field_valid( pr_st40.*, "status", p_err_src ) RETURNING l_close_view

            IF ( l_close_view = FALSE ) THEN 
                CONTINUE DIALOG 
            END IF 

            IF ( pr_st40.status <> mr_doc_st40.status ) THEN 
                CALL upd_status( pr_st40.*, mr_doc_st40.*, p_err_src )
            END IF       
            
            EXIT DIALOG

            
	END DIALOG
	
	CLOSE WINDOW w_doc_enq
    
-- Rebuild DATA
	CALL bld_and_display_data( NULL,"default_order", p_err_src )
		
END FUNCTION
{==================================================================================================================================}
#+ VALIDATION FUNCTION
#+
#+ BUSINESS RULE:
#+ Validate Fields 
#+
#+ @code CALL w_doc_is_field_valid( p_err_src )
#+
#+ @param p_field_name Field to be validated
#+ @param p_arr_curr passed the current array position
#+ @param p_err_src Error tracking variable
#+
#+ @return  NONE
#+
#+ CHANGES
#+
FUNCTION w_doc_is_field_valid( pr_st40, p_field_name, p_err_src )

	DEFINE	pr_st40             RECORD LIKE st40_track_store_pulling.*,
            p_field_name		STRING,
			p_err_src			STRING,

			l_dlog				ui.Dialog

	LET p_err_src = p_err_src , " > w_doc_is_field_valid"

	LET l_dlog = ui.Dialog.getCurrent()

	---------------------------------------------------------------------------------------------------
	IF ( p_field_name = "all" OR p_field_name = "status" ) THEN
		IF (mr_doc_st40.status IS NULL) THEN 
            CALL fgl_winmessage(%"Invalid Input",%"Phase cannot be blank!","exclamation")
            CALL l_dlog.nextField("status")
            RETURN FALSE 
		END IF 
	END IF
    
    ---------------------------------------------------------------------------------------------------
	IF ( p_field_name = "all" ) OR ( p_field_name = "action_by_user" ) THEN
        -- only do this check if the status has changed and the 'change to' status is a force assign user phase. 
        IF ( pr_st40.status <> mr_doc_st40.status ) AND ( lf_row_exists( "st40u_warehouse_user", "whs_phase_user = 'Y' AND type = 'R' AND force_assign_user = 'Y' AND priority_level LIKE '%"||mr_doc_st40.priority_lvl||"%' AND whs_status = '"||mr_doc_st40.status||"'", p_err_src ) ) THEN 

            IF ( mr_doc_st40.action_by_user IS NULL ) THEN 
                CALL fgl_winmessage(%"Invalid Input",%"Action by cannot be blank!","exclamation")
                CALL w_doc_enq_lookups( p_err_src ) 
                CALL l_dlog.nextField("action_by_user")
                RETURN FALSE 
            END IF

            IF NOT( lf_row_exists( "st40u_warehouse_user", "user_name = '" || mr_doc_st40.action_by_user || "' AND loc IN ('"||gr_sy02.default_loc||"','00') AND whs IN ('"||gr_sy02.default_whs||"','00') AND (type = 'R' OR type = 'B' ) AND whs_phase_user = 'N'", p_err_src ) ) THEN
                CALL fgl_winmessage(%"Invalid Input",%"Warehouse User name does not exist!","exclamation")
                CALL w_doc_enq_lookups( p_err_src ) 
                CALL l_dlog.nextField("action_by_user")
                RETURN FALSE 
            END IF
        END IF 
        
    END IF
    
   --------------------------------------------------------------------------------------------------- 
	RETURN TRUE 
    
END FUNCTION 


{==================================================================================================================================}
{==================================================================================================================================}
{
													END ENQUIRY FUNCTIONS
}
{==================================================================================================================================}
{==================================================================================================================================}
{==================================================================================================================================}
{==================================================================================================================================}

{==================================================================================================================================}
{==================================================================================================================================}
--##################################################################################################################################
{
													INPUT FUNCTIONS
} 
--##################################################################################################################################
{==================================================================================================================================}
{==================================================================================================================================}
FUNCTION INPUT_FUNCTIONS()
END FUNCTION

{==================================================================================================================================}
#+ LOG AN ACTION
#+
#+ BUSINESS RULE:
#+ Allow user to log an action
#+
#+
#+ @code CALL w_log_action( p_doc_no, p_cur_status, p_action_by, p_err_src )
#+
#+ @param   p_doc_no            Current Doc NO.
#+ @param   p_cur_status        Current Document Status
#+ @param   p_action_by         What phase user is current actioned on the Doc
#+ @param   p_err_src           Error tracking variable
#+
#+ @return NONE
#+
#+ CHANGES
#+
FUNCTION w_log_action( p_doc_no, p_cur_status, p_action_by, p_err_src )

	DEFINE	p_doc_no        LIKE st40_track_store_pulling.doc_no,
            p_cur_status    LIKE st40_track_store_pulling.status,
            p_action_by     LIKE st40_track_store_pulling.action_by,
            p_err_src		STRING,

			l_ok_to_update	BOOLEAN,
            l_next_phase    STRING

	LET p_err_src = p_err_src , " > w_log_action"

--Reset Action logging variables
	INITIALIZE mr_st40_action.* TO NULL
	
	LET m_complete_task = FALSE

	CALL lf_load_styles( p_err_src )
	OPEN WINDOW w_action WITH FORM "st143_warehouse_action"

    -- Set Defaults:
    CALL lf_doc_type_build( p_doc_no[1,11], p_err_src ) RETURNING mr_st40_action.doc_type
    
    -- Build two display texts 'Current Phase' && 'Next Phase' 
    CALL sb_cbox_build_action( p_doc_no[1,11], p_action_by, p_cur_status )
    LET mr_st40_action.doc_no       = p_doc_no[1,11]
    LET mr_st40_action.ship_doc_no  = p_doc_no[12,13]
    
    -- Depending if the current phase is force_assign_user -> if it is then we set the user_name to NULL
    IF ( lf_row_exists( "st40u_warehouse_user","whs_phase_user = 'Y' AND type = 'R' AND whs_phase_sort = '"||p_cur_status||"' AND force_assign_user = 'Y'", p_err_src ) ) THEN
        LET mr_st40_action.user_name = NULL
    ELSE    
        LET l_next_phase = lf_get_field_value( "st40u_warehouse_user", "FIRST 1 full_name","whs_phase_user = 'Y' AND type = 'R' AND whs_phase_sort > '"||p_cur_status||"' AND po_hold = 'N' AND cd_hold = 'N' ORDER BY whs_phase_sort", p_err_src )

        IF NOT( lf_row_exists( "st40u_warehouse_user","whs_phase_user = 'Y' AND type = 'R' AND full_name = '"||l_next_phase||"' AND force_assign_user = 'Y'", p_err_src ) ) THEN
            LET mr_st40_action.user_name = p_action_by
            LET mr_st40_action.full_name = lf_get_field_value( "st40u_warehouse_user", "full_name", "user_name = '" || mr_st40_action.user_name || "' AND loc IN ('"||gr_sy02.default_loc||"','00') AND whs IN ('"||gr_sy02.default_whs||"','00') AND (type = 'R' OR type = 'B')", p_err_src )
        END IF 
        
    END IF 
    
	DIALOG ATTRIBUTES( FIELD ORDER FORM, UNBUFFERED )
		INPUT BY NAME mr_st40_action.* ATTRIBUTES( WITHOUT DEFAULTS )
		-----------------------------------------------------------------------
			BEFORE INPUT
				CALL DIALOG.setFieldActive( "full_name",	FALSE )
                IF ( mr_st40_action.doc_no IS NOT NULL ) THEN 
                    CALL DIALOG.nextField("user_name")
                END IF 
                IF ( mr_st40_action.doc_type <> "IBT" ) THEN 
                    CALL DIALOG.setFieldActive("ship_doc_no",           FALSE  )
                ELSE 
                    CALL DIALOG.setFieldActive("ship_doc_no",           TRUE   )
                END IF
                
		-----------------------------------------------------------------------
			AFTER FIELD user_name
				CALL w_log_action_is_field_valid( "user_name", p_err_src ) RETURNING g_not_used

        -----------------------------------------------------------------------
            ON CHANGE doc_type 
                LET mr_st40_action.doc_no       = NULL
                LET mr_st40_action.full_name    = NULL 
                LET mr_st40_action.user_name    = NULL
                LET mr_st40_action.ship_doc_no  = NULL
                DISPLAY "" TO cur_phase
                DISPLAY "" TO next_phase
                
                IF ( mr_st40_action.doc_type = "IBT" ) THEN 
                    CALL DIALOG.setFieldActive("ship_doc_no",           TRUE  )
                ELSE 
                    CALL DIALOG.setFieldActive("ship_doc_no",           FALSE )
                END IF
                
		-----------------------------------------------------------------------
			AFTER FIELD doc_type            
                IF ( mr_st40_action.doc_no IS NULL ) THEN 
                    CALL DIALOG.nextField("doc_no") 
                END IF 
				CALL w_log_action_is_field_valid( "doc_type", p_err_src ) RETURNING g_not_used

		-----------------------------------------------------------------------
			AFTER FIELD doc_no
				CALL w_log_action_is_field_valid( "doc_no", p_err_src ) RETURNING g_not_used
                
		-----------------------------------------------------------------------
			AFTER FIELD ship_doc_no
				CALL w_log_action_is_field_valid( "ship_doc_no", p_err_src ) RETURNING g_not_used

		-----------------------------------------------------------------------
			AFTER INPUT
				IF NOT( w_log_action_is_field_valid( "all", p_err_src ) ) THEN
					NEXT FIELD CURRENT
				END IF
				
				LET l_ok_to_update = TRUE
				EXIT DIALOG
				
		END INPUT

    ---------------------------------------------------------------------------
        BEFORE DIALOG
            --========================== CHECK FOR SERIAL ALLOC ==========================--
            -- Make sure the current PHASE has ibt_con
            IF ( lf_row_exists( "st40u_warehouse_user", "whs_phase_user = 'Y' AND chg_put_bin = 'Y' AND whs_phase_sort = '"||p_cur_status||"' AND type = 'R'", p_err_src ) ) THEN 
                CALL DIALOG.setActionActive( "bin_allocation", TRUE  )
                CALL DIALOG.setActionHidden( "bin_allocation", FALSE )
            ELSE 
                CALL DIALOG.setActionActive( "bin_allocation", FALSE )
                CALL DIALOG.setActionHidden( "bin_allocation", TRUE )   
            END IF 
            --===========================================================================--

	---------------------------------------------------------------------------
		ON ACTION field_lookup
			CALL w_log_action_lookups( NULL, p_err_src )
			
	---------------------------------------------------------------------------
		ON ACTION ACCEPT
			ACCEPT DIALOG

    ---------------------------------------------------------------------------
        ON ACTION bin_allocation
            CALL lf_bin_warehouse_allocation( "R",p_doc_no, mr_st40_action.doc_type, gr_sy02.default_loc, gr_sy02.default_whs, p_err_src ) RETURNING g_not_used

	---------------------------------------------------------------------------
		ON ACTION CANCEL
			LET l_ok_to_update = FALSE
			EXIT DIALOG
			
	END DIALOG
	
	CLOSE WINDOW w_action

	IF ( l_ok_to_update ) THEN	
        IF ( lf_row_exists( "st40u_warehouse_user", "user_name = '"|| mr_st40_action.user_name ||"' AND allow_label_prt = 'Y' AND type = 'R'", p_err_src ) ) THEN 
            RUN "fglrun " ||gr_st00.form_package_prnt||" "||  g_schema_name || " " || g_user_name || " Z "||mr_st40_action.doc_no || " Z " WITHOUT WAITING
        END IF 
        
		CALL upd_log_action( p_err_src )

		CALL bld_and_display_data( NULL,"default_order", p_err_src )
        CALL ui.Interface.refresh()
	END IF

END FUNCTION


{==================================================================================================================================}
#+ BUTTON LOOKUPS
#+
#+ BUSINESS RULE:
#+ Lookup function for button lookups
#+
#+ @code CALL w_log_action_lookups( p_field_name, p_err_src )
#+
#+ @param   p_field_name    Field to be validated
#+ @param   p_err_src       Error tracking variable
#+
#+ @return  NONE
#+
#+ CHANGES
#+
FUNCTION w_log_action_is_field_valid( p_field_name, p_err_src )

	DEFINE	p_field_name		STRING,
			p_err_src			STRING,

			l_last_phase		INTEGER,
			l_last_action		LIKE st40u_warehouse_user.full_name,
			l_dlog				ui.Dialog,
            l_next_phase        STRING,
            l_doc_no            VARCHAR(14)

	LET p_err_src = p_err_src , " > w_log_action_is_field_valid"

	LET l_dlog = ui.DIALOG.getCurrent()

---------------------------------------------------------------------------------------------------
	IF ( p_field_name = "all" OR p_field_name = "user_name" ) THEN
		IF ( mr_st40_action.user_name IS NULL ) THEN
            CALL w_log_action_lookups( NULL, p_err_src )
            CALL l_dlog.nextField( "user_name" )
			RETURN FALSE
		END IF

		IF NOT( lf_row_exists( "st40u_warehouse_user", "user_name = '" || mr_st40_action.user_name || "' AND loc IN ('"||gr_sy02.default_loc||"','00') AND whs IN ('"||gr_sy02.default_whs||"','00') AND (type = 'R' OR type = 'B' )", p_err_src ) ) THEN
            CALL w_log_action_lookups( NULL, p_err_src )
            CALL l_dlog.nextField( "user_name" )
			RETURN FALSE
		END IF

		LET mr_st40_action.full_name 		= lf_get_field_value( "st40u_warehouse_user", "full_name", "user_name = '" || mr_st40_action.user_name || "' AND loc IN ('"||gr_sy02.default_loc||"','00') AND whs IN ('"||gr_sy02.default_whs||"','00') AND (type = 'R' OR type = 'B')", p_err_src )

	END IF

---------------------------------------------------------------------------------------------------
	IF ( p_field_name = "all" OR p_field_name = "doc_type" ) THEN
		IF ( mr_st40_action.doc_type IS NULL ) THEN
			CALL fgl_winmessage( %"Invalid Input", %"Please select a Document Type!", "exclamation" )
			CALL l_dlog.nextField( "doc_type" )
			RETURN FALSE
		END IF
	END IF

---------------------------------------------------------------------------------------------------
    IF ( p_field_name = "all" OR p_field_name = "doc_no" ) THEN
		IF ( mr_st40_action.doc_no IS NULL ) THEN
            CALL w_log_action_lookups( NULL, p_err_src )
            CALL l_dlog.nextField( "doc_no" )
			RETURN FALSE
		END IF
	{
	Default the Next Action according to the following rules
		1. Customer Collection - Priority 1 or 2
			Unassigned > Being Pulled > Checker > Collection Ready > Collected
			
		2. 3rd Party Delivery - Priority 3
			Unassigned > Being Pulled > Checker > Collection Ready > Collected
			
		3. Own Transport - Priority 4
			Unassigned > Being Pulled > Checker > Transport Controller > On Delivery > Delivered
	}
        CASE ( mr_st40_action.doc_type )
            WHEN "CRN"
                IF NOT( lf_row_exists( "st40_track_store_pulling", "doc_type = 'CRN' AND doc_no = '" || mr_st40_action.doc_no || "' AND loc IN ('"||gr_sy02.default_loc||"','00') AND whs IN ('"||gr_sy02.default_whs||"','00')", p_err_src ) ) THEN
                    CALL w_log_action_lookups( NULL, p_err_src )
                    CALL l_dlog.nextField( "doc_no" )
                    RETURN FALSE
                END IF	

            WHEN "IBT"
                IF ( mr_st40_action.ship_doc_no IS NULL ) THEN 
                    IF NOT( lf_row_exists( "ib24_ib_hd", "ibt_no IN (SELECT doc_no[1,11] FROM st40_track_store_pulling WHERE doc_type = 'IBT' AND loc IN ('"||gr_sy02.default_loc||"','00') AND whs IN ('"||gr_sy02.default_whs||"','00') AND doc_no[1,11] = '" || mr_st40_action.doc_no||"' )", p_err_src ) ) THEN
                        CALL w_log_action_lookups( NULL, p_err_src )
                        CALL l_dlog.nextField( "doc_no" )
                        RETURN FALSE
                    END IF

                    -- Make sure the user gets put into the ship doc field
                    CALL l_dlog.nextField( "ship_doc_no" )
                    RETURN FALSE
                ELSE 
                
                    IF NOT( lf_row_exists( "ib24_ib_hd", "ibt_no IN (SELECT doc_no[1,11] FROM st40_track_store_pulling WHERE doc_type = 'IBT' AND loc IN ('"||gr_sy02.default_loc||"','00') AND whs IN ('"||gr_sy02.default_whs||"','00') AND doc_no = '" || mr_st40_action.doc_no||","||mr_st40_action.ship_doc_no||"' )", p_err_src ) ) THEN
                        CALL w_log_action_lookups( NULL, p_err_src )
                        CALL l_dlog.nextField( "doc_no" )
                        RETURN FALSE
                    END IF
                END IF 
                
            WHEN "COL"
                IF NOT( lf_row_exists( "st40_track_store_pulling", "doc_type = 'COL' AND doc_no = '" || mr_st40_action.doc_no || "' AND loc IN ('"||gr_sy02.default_loc||"','00') AND whs IN ('"||gr_sy02.default_whs||"','00')", p_err_src ) ) THEN
                    CALL w_log_action_lookups( NULL, p_err_src )
                    CALL l_dlog.nextField( "doc_no" )
                    RETURN FALSE
                END IF

            WHEN "PO"
                IF NOT( lf_row_exists( "st40_track_store_pulling", "doc_type = 'PO' AND doc_no = '" || mr_st40_action.doc_no || "' AND loc IN ('"||gr_sy02.default_loc||"','00') AND whs IN ('"||gr_sy02.default_whs||"','00')", p_err_src ) ) THEN
                    CALL w_log_action_lookups( NULL, p_err_src )
                    CALL l_dlog.nextField( "doc_no" )
                    RETURN FALSE
                END IF

            WHEN "GRN"
                IF NOT( lf_row_exists( "st40_track_store_pulling", "doc_type = 'GRN' AND doc_no = '" || mr_st40_action.doc_no || "' AND loc IN ('"||gr_sy02.default_loc||"','00') AND whs IN ('"||gr_sy02.default_whs||"','00')", p_err_src ) ) THEN
                    CALL w_log_action_lookups( NULL, p_err_src )
                    CALL l_dlog.nextField( "doc_no" )
                    RETURN FALSE
                END IF

            WHEN "IBR"
                IF NOT( lf_row_exists( "st40_track_store_pulling", "doc_type = 'IBR' AND doc_no = '" || mr_st40_action.doc_no || "' AND loc IN ('"||gr_sy02.default_loc||"','00') AND whs IN ('"||gr_sy02.default_whs||"','00')", p_err_src ) ) THEN
                    CALL w_log_action_lookups( NULL, p_err_src )
                    CALL l_dlog.nextField( "doc_no" )
                    RETURN FALSE
                END IF

            WHEN "IBT"
                IF NOT( lf_row_exists( "st40_track_store_pulling", "doc_type = 'IBT' AND doc_no = '" || mr_st40_action.doc_no || "' AND loc IN ('"||gr_sy02.default_loc||"','00') AND whs IN ('"||gr_sy02.default_whs||"','00')", p_err_src ) ) THEN
                    CALL w_log_action_lookups( NULL, p_err_src )
                    CALL l_dlog.nextField( "doc_no" )
                    RETURN FALSE
                END IF

            WHEN "BOM"
                IF NOT( lf_row_exists( "st40_track_store_pulling", "doc_type = 'BOM' AND doc_no = '" || mr_st40_action.doc_no || "' AND loc IN ('"||gr_sy02.default_loc||"','00') AND whs IN ('"||gr_sy02.default_whs||"','00')", p_err_src ) ) THEN
                    CALL w_log_action_lookups( NULL, p_err_src )
                    CALL l_dlog.nextField( "doc_no" )
                    RETURN FALSE
                END IF

            OTHERWISE
                CALL fgl_winmessage( %"Invalid Input", %"Please select a Document Type!", "exclamation" )
                CALL l_dlog.nextField( "doc_type" )
                RETURN FALSE

        END CASE
        
	--Fetch original row
        IF ( mr_st40_action.doc_type = "IBT" ) THEN 
            LET l_doc_no = mr_st40_action.doc_no||","||mr_st40_action.ship_doc_no
        ELSE 
            LET l_doc_no = mr_st40_action.doc_no
        END IF 
        
		PREPARE qry_st40_org FROM	"SELECT "							||
											"* "						||
										"FROM "							||
											"st40_track_store_pulling "	||
									"WHERE "							||
											"doc_type"	||" = '"|| mr_st40_action.doc_type	||"'"	||" AND "||
											"doc_no"	||" = '"|| l_doc_no	                ||"'"	||" AND "||
											"row_id"	||" = (SELECT MIN(row_id) FROM st40_track_store_pulling WHERE doc_type = '"|| mr_st40_action.doc_type||"' AND doc_no = '"|| l_doc_no||"' AND priority_lvl = '5')"
		EXECUTE qry_st40_org INTO mr_st40_org.*

	--Only allow the current whs_phase_user to action the document.
		LET l_last_phase 	= lf_get_field_value( 	{tbl_name}		"st40_track_store_pulling", 
													{field_name}	"FIRST 1(whs_phase_sort)", 
													{where_clause}	"doc_type = '" || mr_st40_action.doc_type || "' AND doc_no = '" || l_doc_no || "' AND priority_lvl = '5' ORDER BY row_id DESC", 
													p_err_src )

		IF ( l_last_phase <> 0 AND l_last_phase <> 1 ) THEN
			LET l_last_action 	= lf_get_field_value( 	{tbl_name}		"st40u_warehouse_user", 
														{field_name}	"full_name", 
														{where_clause}	"whs_phase_sort = '" || l_last_phase || "' AND type = 'R'",
														p_err_src )
		
			IF ( lf_row_exists(	{tbl_name}		"st40_track_store_pulling",
								{where_clause}	"doc_type"			||" = "		|| "'"	|| mr_st40_action.doc_type			||"'"	||" AND "||
												"doc_no"			||" = "		|| "'"	|| l_doc_no			                ||"'"	||" AND "||
												"end_date"			||" IS "	|| "NULL"											||" AND "||
                                                "priority_lvl = '5'"                                                                ||" AND "||
												"action_by"			||" <> "	|| "'"	|| mr_st40_action.user_name 		||"'",
								{err_src}		p_err_src ) )
			THEN
                -- Then make sure that the current phase is NOT a force_assigned_user
                IF NOT ( lf_row_exists(	"st40u_warehouse_user", "whs_phase_user = 'Y' AND whs_phase_sort = " || l_last_phase ||" AND force_assign_user = 'Y' AND type = 'R' AND priority_level like '%"||mr_st40_org.priority_lvl||"%'", p_err_src ) )THEN 

                    CALL fgl_winmessage( %"Invalid Input", %"Document '"||mr_st40_action.doc_no||"' is currently being actioned by "|| l_last_action  ||"!\nOnly the "||l_last_action||" can log the next action on the document.", "exclamation" )
                    CALL l_dlog.nextField( "doc_type" )
                    RETURN FALSE
                END IF 
			END IF
		END IF

	--Ensure that a special user is NOT opening up a new task Only if the next phase is NOT force_assign_user phase
		IF ( 
			lf_row_exists( "st40u_warehouse_user", "whs_phase_user = 'Y' AND user_name = '" || mr_st40_action.user_name || "' AND type = 'R'", p_err_src ) AND
            lf_row_exists( "st40u_warehouse_user", "whs_phase_user = 'Y' AND user_name = '" || mr_st40_action.user_name || "' AND type = 'R'", p_err_src )

			)
		THEN
			IF NOT( lf_row_exists(	{tbl_name}		"st40_track_store_pulling",
									{where_clause}	"doc_no"	||" = "	|| "'"|| l_doc_no	              	||"'"	||" AND "||
													"loc IN ('"||gr_sy02.default_loc||"','00')"						||" AND "||
													"whs IN ('"||gr_sy02.default_whs||"','00')"						||" AND "||
													"end_date"	||" IS "||		 "NULL"								||" AND "||
													"action_by"	||" = "	|| "'"|| mr_st40_action.user_name	||"'",
									{err_src}		p_err_src ) )
			THEN
				CALL fgl_winmessage( %"Invalid Input", %"Special users cannot open new tasks!", "exclamation" )
                CALL l_dlog.nextField( "user_name" )
                RETURN FALSE
			END IF
		END IF
		
	END IF
    
---------------------------------------------------------------------------------------------------
	IF (( p_field_name = "all" ) OR ( p_field_name = "ship_doc_no" )) AND ( mr_st40_action.doc_type = "IBT" ) THEN
        IF ( mr_st40_action.ship_doc_no IS NULL ) THEN 
            CALL fgl_winmessage( %"Invalid Input", %"This Shipping Document No. cannot be empty!", "exclamation" )
            CALL l_dlog.nextField( "ship_doc_no" )
            RETURN FALSE
        END IF 
        
        IF NOT( lf_row_exists( "ib30_ship_doc_hd", "ibt_no = '" || mr_st40_action.doc_no || "' AND ship_doc_no = '" || mr_st40_action.ship_doc_no || "'", p_err_src ) ) THEN
            CALL fgl_winmessage( %"Invalid Input", %"This Shipping Document No(" || mr_st40_action.ship_doc_no || ") does not Exist.\nPlease enter in a valid Shipping Doc No.", "exclamation" )
            CALL l_dlog.nextField( "ship_doc_no" )
            RETURN FALSE
        END IF

	END IF
    
---------------------------------------------------------------------------------------------------
	IF ( p_field_name = "all" ) THEN
        LET l_next_phase = lf_get_field_value( "st40u_warehouse_user", "FIRST 1 whs_phase_sort","whs_phase_user = 'Y' AND type = 'R' AND whs_phase_sort > '"||l_last_phase||"' AND po_hold = 'N' AND cd_hold = 'N' ORDER BY whs_phase_sort", p_err_src )

    --Check to make sure that that the next phase is 'Foce Assigned User Y/N' if it is Yes then we force the user actioning to be a 
    --NON phase user. Eg 'catop'. If if force_assign_user = 'N' then make sure the a whs_phase_user = 'Y' actions the document, EG 'dispatchck'
        IF ( lf_row_exists( "st40u_warehouse_user", "force_assign_user = 'Y' AND whs_phase_user = 'Y' AND whs_phase_sort IN('"||l_last_phase||"','"||l_next_phase||"') AND priority_level = '5' AND type = 'R'", p_err_src)) THEN 

            -- Make sure the action_by is NOT a phase user. if its a warehouse Phase user then block
            IF ( lf_row_exists ("st40u_warehouse_user","whs_phase_user = 'Y' AND user_name = '"||mr_st40_action.user_name||"' ANd loc IN ('"||gr_sy02.default_loc||"','00') AND whs IN ('"||gr_sy02.default_whs||"','00') AND type = 'R'", p_err_src) ) THEN 
                CALL fgl_winmessage( %"Invalid Action", %"This phase has 'Force Assigned User' enabled for it and cannot be actioned by a Phase User.\nPlease select a Non-Phase user to action this phase.", "exclamation" )
                RETURN FALSE 
            END IF 
        ELSE
            -- First check if the the current phase is force_assign_user = 'Y' and that you are completing the user logged phase
            IF ( sb_force_assign_user_true( FALSE, l_last_phase, gr_sy02.default_loc, gr_sy02.default_whs, mr_st40_action.doc_type, mr_st40_action.doc_no, p_err_src ) ) THEN 
                -- Do a count of how many st40 enteries there are. If this is the last one then we complete and move to the next phase
            ELSE 
                -- Make sure the action_by IS a phase user. if its a warehouse user then block
                IF ( lf_row_exists ("st40u_warehouse_user","whs_phase_user = 'N' AND user_name = '"||mr_st40_action.user_name||"' AND loc IN ('"||gr_sy02.default_loc||"','00') AND whs IN ('"||gr_sy02.default_whs||"','00') AND (type = 'R' OR type = 'B' )", p_err_src) ) THEN 
                    CALL fgl_winmessage( %"Invalid Action", %"This phase cannot be actioned by a Non-Phase user as it does not have 'Force Assigned User' enabled.\nPlease select a phase user to action this phase.", "exclamation" )
                    RETURN FALSE
                END IF 
            END IF 
        END IF 
        
	--Check if user is busy with current document
		IF ( lf_row_exists(	{tbl_name}		"st40_track_store_pulling",
							{where_clause}	"doc_no"	||" = "	|| "'"|| mr_st40_action.doc_no		||"'"	||" AND "||
											"end_date"	||" IS "||		 "NULL"								||" AND "||
											"action_by"	||" = "	|| "'"|| mr_st40_action.user_name	||"'",
							{err_src}		p_err_src ) )
		THEN
			--Check if any other outstanding tasks exist for the current document 
            IF ( lf_row_exists(	{tbl_name}		"st40_track_store_pulling",
                                {where_clause}	"doc_type = '"  || mr_st40_action.doc_type													    ||"'"	||" AND "||
                                                "doc_no = '"    || mr_st40_action.doc_no														||"'"	||" AND "||
                                                "action_by IN (SELECT user_name FROM st40u_warehouse_user WHERE whs_phase_user = 'N' and user_name <> '"|| mr_st40_action.user_name	||"')"	||" "	||" AND "||
                                                "action_by <> (SELECT user_name FROM st40u_warehouse_user WHERE trip_plan = 'Y' AND type = 'R')"||" "	||" AND "||
                                                "end_date IS NULL"																		        ||" AND "||
                                                "action_by <> '"|| mr_st40_action.user_name													    ||"'",
                                {err_src}		p_err_src ) )
            THEN 
            --Prompt the user that there are still outstanding tasks for this document
                CALL fgl_winmessage( %"Outstanding Tasks", %"There are still outstanding tasks for this document.", "exclamation" )
            END IF
            LET m_complete_task = TRUE
            RETURN TRUE

		END IF

		IF ( m_complete_task ) THEN
		--Continue Input - Failsafe - Should never get here
			RETURN FALSE
		ELSE
            RETURN TRUE

		END IF
	ELSE
		RETURN TRUE
	END IF

END FUNCTION

{==================================================================================================================================}
#+ BUTTON LOOKUPS
#+
#+ BUSINESS RULE:
#+ Lookup function for button lookups
#+
#+ @code CALL w_log_action_lookups( p_action_selected, p_err_src )
#+
#+ @param p_action_selected Action selected
#+ @param p_err_src
#+
#+ @return  NONE
#+
#+ CHANGES
#+
FUNCTION w_log_action_lookups( p_action_selected, p_err_src )

	DEFINE  p_action_selected	STRING,
			p_err_src			STRING,

			l_dlog				ui.dialog,
            l_loc_name          STRING,
            l_whs_name          STRING,
            l_key_variable      STRING

    DEFINE  la_fields           DYNAMIC ARRAY OF STRING 
    
	LET p_err_src = p_err_src , " > w_log_action_lookups"
			
    LET l_dlog = ui.dialog.getcurrent()
    IF ( m_loc_name IS NULL ) THEN 
        LET l_loc_name = gr_sy02.default_loc
    ELSE 
        LET l_loc_name = m_loc_name
    END IF 
    
    IF ( m_whs_name IS NULL ) THEN 
        LET l_whs_name = gr_sy02.default_whs
    ELSE 
        LET l_whs_name = m_whs_name
    END IF 
    
    CASE
		WHEN ( l_dlog.getCurrentItem() = "user_name" )
			LET mr_st40_action.user_name = lf_lookup(	{tbl_name}          "st40u_warehouse_user",
														{cols}              "loc, whs, user_name, full_name",
														{col_titles}        "LOC, WHS, Username, Full Name ",
														{fld_att}           "V3,V3,V10,V25",
														{flds_to_search}    "user_name",
														{flds_to_return}	"3",
														{input_string}      mr_st40_action.user_name,
														{sub_qry}           "loc IN ('"|| l_loc_name ||"','00') AND whs IN ('"|| l_whs_name ||"','00') AND ( type = 'R' OR type = 'B' )",
														{ord_by}            "loc,whs_phase_sort,user_name",
														{win_title}         "Warehouse User Lookup",
														{err_src}			p_err_src)
                                                        
		WHEN ( l_dlog.getCurrentItem() = "doc_no" )
			CASE ( mr_st40_action.doc_type )
				WHEN "CRN"
					LET mr_st40_action.doc_no = lf_lookup_code(	{code}				"sa25_inv_hd",
																{input_val}			mr_st40_action.doc_no,
																{parent_code}		NULL,
																{loc}				NULL,
																{p_where_clause}	"doc_type = 'I' AND inv_type = 'C' AND doc_no IN (SELECT doc_no FROM st40_track_store_pulling WHERE loc IN ('"|| l_loc_name ||"','00') AND whs IN ('"|| l_whs_name ||"','00') AND doc_type = 'CRN' AND end_date IS NULL)",
																{err_src}			p_err_src )												

				WHEN "IBT"
					LET mr_st40_action.doc_no = lf_lookup(		{tbl_name}          "ib24_ib_hd",
																{cols}              "ibt_no,send_loc,send_whs,rec_loc,rec_whs",
																{col_titles}        "IBT No,Send,Whs,Rec,Whs",
																{fld_att}           "V16,V4,V3,V4,V3",
																{flds_to_search}    "ibt_no,send_loc,send_whs,rec_loc,rec_whs",
																{flds_to_return}	"1",
																{input_string}      mr_st40_action.doc_no,
																{sub_qry}           "ibt_no IN (SELECT doc_no[1,11] FROM st40_track_store_pulling WHERE doc_type = 'IBT' AND end_date IS NULL AND loc IN ('"|| l_loc_name ||"','00') AND whs IN ('"|| l_whs_name ||"','00') AND priority_lvl = 5 )",
																{ord_by}            "ibt_no",
																{win_title}         "IBT Lookup",
																{err_src}			p_err_src )

				WHEN "COL"
					LET mr_st40_action.doc_no = lf_lookup(		{tbl_name}          "st40_track_store_pulling",
																{cols}              "DISTINCT loc,whs,doc_type,doc_no",
																{col_titles}        "Loc,Whs,Type,Doc No",
																{fld_att}           "V3,V3,V3,V10",
																{flds_to_search}    "loc,whs,doc_type,doc_no",
																{flds_to_return}	"4",
																{input_string}      NULL,
																{sub_qry}           "doc_type = 'COL' AND end_date IS NULL AND loc IN ('"|| l_loc_name ||"','00') AND whs IN ('"|| l_whs_name ||"','00') AND priority_lvl = 5",
																{ord_by}            "doc_no",
																{win_title}         "COLLECTION Lookup",
																{err_src}			p_err_src )

				WHEN "GRN"
					LET mr_st40_action.doc_no = lf_lookup(		{tbl_name}          "st40_track_store_pulling",
																{cols}              "DISTINCT loc,whs,doc_type,doc_no",
																{col_titles}        "Loc,Whs,Type,Doc No",
																{fld_att}           "V3,V3,V3,V10",
																{flds_to_search}    "loc,whs,doc_type,doc_no",
																{flds_to_return}	"4",
																{input_string}      NULL,
																{sub_qry}           "doc_type = 'GRN' AND end_date IS NULL AND loc IN ('"|| l_loc_name ||"','00') AND whs IN ('"|| l_whs_name ||"','00') AND priority_lvl = 5",
																{ord_by}            "doc_no",
																{win_title}         "GRN Lookup",
																{err_src}			p_err_src )

				WHEN "PO"
					LET mr_st40_action.doc_no = lf_lookup_code(	{code}				"pu22_po_hd",
																{input_val}			mr_st40_action.doc_no,
																{parent_code}		NULL,
																{loc}				NULL,
																{p_where_clause}	"doc_type = 'P' AND status = 'O' AND doc_no IN (SELECT doc_no FROM st40_track_store_pulling WHERE doc_type = 'PO' AND end_date IS NULL AND loc IN ('"|| l_loc_name ||"','00') AND whs IN ('"|| l_whs_name ||"','00') )",
																{err_src}			p_err_src )

				WHEN "GRV"
					LET mr_st40_action.doc_no = lf_lookup(	{tbl_name}			"pu25_grn_hd",
                                                            {cols}				"doc_no,cl_code,cl_name",
                                                            {col_title}			"GRN No,Account,Name",
                                                            {fld_att}			"V11,V6,V40",
                                                            {flds_to_search}	"doc_no,cl_code,cl_name",
                                                            {flds_to_return}	"1",
                                                            {input_string}		NULL,
                                                            {sub_qry}			"doc_type = 'C' AND doc_no IN (SELECT doc_no FROM st40_track_store_pulling WHERE doc_type = 'GRV' AND loc IN ('"|| l_loc_name ||"','00') AND whs IN ('"|| l_whs_name ||"','00') )",
                                                            {ord_by}			"doc_no",
                                                            {win_title}			"GRV No. Lookup",
                                                            {err_src}			p_err_src )

				WHEN "IBR"
					LET mr_st40_action.doc_no = lf_lookup(	{tbl_name}          "ib20_req_hd",
															{cols}              "req_no,req_loc,send_loc,request_date,required_date",
															{col_titles}        "Request No,Req Loc,From Loc,Requested,Required",
															{fld_att}           "V16,V4,V4,T10,T10",
															{flds_to_search}    "req_no,req_loc,send_loc",
															{flds_to_return}	"1",
															{input_string}      NULL,
															{sub_qry}           "status = 'O' AND req_no IN (SELECT doc_no FROM st40_track_store_pulling WHERE doc_type = 'IBR' AND loc IN ('"|| l_loc_name ||"','00') AND whs IN ('"|| l_whs_name ||"','00') )",
															{ord_by}            "req_no",
															{win_title}         "IB Request Lookup",
															{err_src}			p_err_src)

				OTHERWISE
					CALL fgl_winmessage( %"Invalid Input", %"Please select a Document Type!", "exclamation" )

			END CASE

		WHEN ( p_action_selected = "doc_find" )
			CASE ( mr_doc_st40.doc_type )
                    
				WHEN "CRN"
                    LET l_key_variable = lf_lookup(	{tbl_name}          "st40_track_store_pulling",
                                                    {cols}              "doc_no, dest_name, priority_lvl, (SELECT full_name FROM st40u_warehouse_user WHERE whs_status = status)",
                                                    {col_titles}        "Doc No., Destination, Priority, Current Phase ",
                                                    {fld_att}           "V11,V20,I5,V20",
                                                    {flds_to_search}    "doc_no",
                                                    {flds_to_return}	"1,3",
                                                    {input_string}      NULL,
                                                    {sub_qry}           "loc = '"|| l_loc_name ||"' AND whs = '"|| l_whs_name ||"' AND doc_type = 'CRN' AND end_date IS NULL AND priority_lvl = 5",
                                                    {ord_by}            "doc_no,priority_lvl, status",
                                                    {win_title}         "Warehouse Credit Note Lookup",
                                                    {err_src}			p_err_src)

                    IF l_key_variable IS NOT NULL THEN 
                        CALL lf_build_array_from_string( l_key_variable, la_fields, "," ) RETURNING g_not_used
                        LET mr_doc_st40.doc_no	        =	la_fields[1]
                        LET mr_doc_st40.priority_lvl	=	la_fields[2]
                    ELSE 
                         LET mr_doc_st40.doc_no         = NULL 
                         LET mr_doc_st40.priority_lvl   = NULL 
                    END IF 

				WHEN "IBT"
                    LET l_key_variable = lf_lookup(	{tbl_name}          "st40_track_store_pulling",
                                                    {cols}              "doc_no, dest_name, priority_lvl, (SELECT full_name FROM st40u_warehouse_user WHERE whs_status = status)",
                                                    {col_titles}        "Doc No., Destination, Priority, Current Phase ",
                                                    {fld_att}           "V11,V20,I5,V20",
                                                    {flds_to_search}    "doc_no",
                                                    {flds_to_return}	"1,3",
                                                    {input_string}      NULL,
                                                    {sub_qry}           "loc = '"|| l_loc_name ||"' AND whs = '"|| l_whs_name ||"' AND doc_type = 'IBT' AND end_date IS NULL AND priority_lvl = 5",
                                                    {ord_by}            "doc_no, priority_lvl, status",
                                                    {win_title}         "Warehouse IBT Lookup",
                                                    {err_src}			p_err_src)
                                                    
                    IF l_key_variable IS NOT NULL THEN 
                        CALL lf_build_array_from_string( l_key_variable, la_fields, "," ) RETURNING g_not_used
                        LET mr_doc_st40.doc_no	        =	la_fields[1]||","||la_fields[2]
                        LET mr_doc_st40.priority_lvl	=	la_fields[3]
                    ELSE 
                         LET mr_doc_st40.doc_no         = NULL 
                         LET mr_doc_st40.priority_lvl   = NULL 
                    END IF 
                    
				WHEN "COL"
                    LET l_key_variable = lf_lookup(	{tbl_name}          "st40_track_store_pulling",
                                                    {cols}              "doc_no, dest_name, priority_lvl, (SELECT full_name FROM st40u_warehouse_user WHERE whs_status = status)",
                                                    {col_titles}        "Doc No., Destination, Priority, Current Phase ",
                                                    {fld_att}           "V11,V20,I5,V20",
                                                    {flds_to_search}    "doc_no",
                                                    {flds_to_return}	"1,3",
                                                    {input_string}      NULL,
                                                    {sub_qry}           "loc = '"|| l_loc_name ||"' AND whs = '"|| l_whs_name ||"' AND doc_type = 'COL' AND end_date IS NULL AND priority_lvl = 5",
                                                    {ord_by}            "doc_no, priority_lvl, status",
                                                    {win_title}         "Warehouse Collection Lookup",
                                                    {err_src}			p_err_src)
                                                        
                    IF l_key_variable IS NOT NULL THEN 
                        CALL lf_build_array_from_string( l_key_variable, la_fields, "," ) RETURNING g_not_used
                        LET mr_doc_st40.doc_no	        =	la_fields[1]
                        LET mr_doc_st40.priority_lvl	=	la_fields[2]
                    ELSE 
                         LET mr_doc_st40.doc_no         = NULL 
                         LET mr_doc_st40.priority_lvl   = NULL 
                    END IF 	
                    
				WHEN "GRN"
                    LET l_key_variable = lf_lookup(	{tbl_name}          "st40_track_store_pulling",
                                                    {cols}              "doc_no, dest_name, priority_lvl, (SELECT full_name FROM st40u_warehouse_user WHERE whs_status = status)",
                                                    {col_titles}        "Doc No., Destination, Priority, Current Phase ",
                                                    {fld_att}           "V11,V20,I5,V20",
                                                    {flds_to_search}    "doc_no",
                                                    {flds_to_return}	"1,3",
                                                    {input_string}      NULL,
                                                    {sub_qry}           "loc = '"|| l_loc_name ||"' AND whs = '"|| l_whs_name ||"' AND doc_type = 'GRN' AND end_date IS NULL AND priority_lvl = 5",
                                                    {ord_by}            "doc_no, priority_lvl, status",
                                                    {win_title}         "Warehouse GRN Lookup",
                                                    {err_src}			p_err_src)
                                                    
                    IF l_key_variable IS NOT NULL THEN 
                        CALL lf_build_array_from_string( l_key_variable, la_fields, "," ) RETURNING g_not_used
                        LET mr_doc_st40.doc_no	        =	la_fields[1]
                        LET mr_doc_st40.priority_lvl	=	la_fields[2]
                    ELSE 
                         LET mr_doc_st40.doc_no         = NULL 
                         LET mr_doc_st40.priority_lvl   = NULL 
                    END IF 	
                       
				WHEN "PO"
                    LET l_key_variable = lf_lookup(	{tbl_name}          "st40_track_store_pulling",
                                                    {cols}              "doc_no, dest_name, priority_lvl, (SELECT full_name FROM st40u_warehouse_user WHERE whs_status = status)",
                                                    {col_titles}        "Doc No., Destination, Priority, Current Phase ",
                                                    {fld_att}           "V11,V20,I5,V20",
                                                    {flds_to_search}    "doc_no",
                                                    {flds_to_return}	"1,3",
                                                    {input_string}      NULL,
                                                    {sub_qry}           "loc = '"|| l_loc_name ||"' AND whs = '"|| l_whs_name ||"' AND doc_type = 'PO' AND end_date IS NULL AND priority_lvl = 5",
                                                    {ord_by}            "doc_no, priority_lvl, status",
                                                    {win_title}         "Warehouse PO Lookup",
                                                    {err_src}			p_err_src)

                    IF l_key_variable IS NOT NULL THEN 
                        CALL lf_build_array_from_string( l_key_variable, la_fields, "," ) RETURNING g_not_used
                        LET mr_doc_st40.doc_no	        =	la_fields[1]
                        LET mr_doc_st40.priority_lvl	=	la_fields[2]
                    ELSE 
                         LET mr_doc_st40.doc_no         = NULL 
                         LET mr_doc_st40.priority_lvl   = NULL 
                    END IF  
                    
				WHEN "IBR"
                    LET l_key_variable = lf_lookup(	{tbl_name}          "st40_track_store_pulling",
                                                    {cols}              "doc_no, dest_name, priority_lvl, (SELECT full_name FROM st40u_warehouse_user WHERE whs_status = status)",
                                                    {col_titles}        "Doc No., Destination, Priority, Current Phase ",
                                                    {fld_att}           "V11,V20,I5,V20",
                                                    {flds_to_search}    "doc_no",
                                                    {flds_to_return}	"1,3",
                                                    {input_string}      NULL,
                                                    {sub_qry}           "loc = '"|| l_loc_name ||"' AND whs = '"|| l_whs_name ||"' AND doc_type = 'IBR' AND end_date IS NULL AND priority_lvl = 5",
                                                    {ord_by}            "doc_no, priority_lvl, status",
                                                    {win_title}         "Warehouse IB Request Lookup",
                                                    {err_src}			p_err_src)

                    IF l_key_variable IS NOT NULL THEN 
                        CALL lf_build_array_from_string( l_key_variable, la_fields, "," ) RETURNING g_not_used
                        LET mr_doc_st40.doc_no	        =	la_fields[1]
                        LET mr_doc_st40.priority_lvl	=	la_fields[2]
                    ELSE 
                         LET mr_doc_st40.doc_no         = NULL 
                         LET mr_doc_st40.priority_lvl   = NULL 
                    END IF     
                    
				OTHERWISE
					CALL fgl_winmessage( %"Invalid Input", %"Please select a Document Type!", "exclamation" )

			END CASE

    END CASE
    
END FUNCTION 


{==================================================================================================================================}
#+ TRANSPORT LOOKUPS
#+
#+ BUSINESS RULE:
#+ Lookup function for button lookups
#+
#+ @code CALL w_tran_action_lookups( p_arr_curr, p_err_src )
#+
#+ @param p_err_src
#+
#+ @return  NONE
#+
#+ CHANGES
#+
FUNCTION w_doc_enq_lookups(p_err_src )

	DEFINE  p_err_src			STRING,
	
			l_dlog				ui.dialog

	LET p_err_src = p_err_src , " > w_doc_enq_lookups"
			
    LET l_dlog = ui.dialog.getcurrent()

    CASE
        WHEN ( l_dlog.getCurrentItem() = "action_by_user" )
			LET mr_doc_st40.action_by_user =      lf_lookup(	{tbl_name}          "st40u_warehouse_user",
                                                                {cols}              "loc, whs, user_name, full_name",
                                                                {col_titles}        "LOC, WHS, Username, Full Name ",
                                                                {fld_att}           "V3,V3,V10,V25",
                                                                {flds_to_search}    "user_name",
                                                                {flds_to_return}	"3",
                                                                {input_string}      mr_doc_st40.action_by_user,
                                                                {sub_qry}           "loc IN ('"|| gr_sy02.default_loc ||"','00') AND whs IN ('"|| gr_sy02.default_whs ||"','00') AND ( type = 'R' OR type = 'B' ) AND whs_phase_user = 'N'",
                                                                {ord_by}            "loc,whs_phase_sort,user_name",
                                                                {win_title}         "Warehouse User Lookup",
                                                                {err_src}			p_err_src)
                                                                
		WHEN ( l_dlog.getCurrentItem() = "area_code" )
			LET mr_doc_st40.area_code =                     lf_lookup(	{tbl_name}          "st41_area_mast",
																		{cols}              "area_code,area_desc,route_no",
																		{col_titles}        "Area Code,Area Description,Route No. ",
																		{fld_att}           "V10,V40,V8",
																		{flds_to_search}    "loc,area_code,area_desc,route_no",
																		{flds_to_return}	"1",
																		{input_string}      mr_doc_st40.area_code,
																		{sub_qry}           "loc = '"||gr_sy02.default_loc||"' AND " ||
																							"whs = '"||gr_sy02.default_whs||"'",
																		{ord_by}            "area_code",
																		{win_title}         "Warehouse Area Code Lookup",
																		{err_src}			p_err_src )

			IF ( mr_doc_st40.area_code IS NOT NULL ) THEN
				CALL l_dlog.setFieldActive( "route_no",	TRUE )
				CALL l_dlog.setFieldActive( "del_order",TRUE )
			END IF
							
		WHEN ( l_dlog.getCurrentItem() = "route_no" )
			LET mr_doc_st40.route_no =                      lf_lookup(	{tbl_name}  		"st41_area_mast",
																		{cols}              "route_no,area_code,area_desc",
																		{col_titles}        "Route No.,Area Code,Area Description ",
																		{fld_att}           "V8,V10,V40",
																		{flds_to_search}    "loc,area_code,area_desc",
																		{flds_to_return}	"1",
																		{input_string}      mr_doc_st40.route_no,
																		{sub_qry}           "loc = '"||gr_sy02.default_loc||"' AND "||
																							"whs = '"||gr_sy02.default_whs||"'",
																		{ord_by}            "route_no",
																		{win_title}         "Warehouse Route No. Lookup",
																		{err_src}			p_err_src )
			
	END CASE 
    
END FUNCTION 

{==================================================================================================================================}
{==================================================================================================================================}
{
													END INPUT FUNCTIONS
}
{==================================================================================================================================}
{==================================================================================================================================}


{==================================================================================================================================}
{==================================================================================================================================}
--##################################################################################################################################
{
													UPDATE FUNCTIONS
} 
--##################################################################################################################################
{==================================================================================================================================}
{==================================================================================================================================}
FUNCTION UPDATE_FUNCTIONS()
END FUNCTION
{==================================================================================================================================}
#+ UPDATE FUNCTION FOR DOCUMENT ENQUIRY
#+
#+ BUSINESS RULE:
#+ Updates the DB for the Document Enquiry changes
#+
#+
#+ @code CALL upd_doc_enq( pr_doc_st40, p_err_src )
#+
#+ @param pr_doc_st40	Parameter record for enquiry when changing priority
#+ @param p_err_src 	Error tracking variable
#+
#+ @return NONE
#+
#+ CHANGES
#+
FUNCTION upd_doc_enq( pr_doc_st40, p_err_src )

	DEFINE pr_doc_st40		RECORD
								doc_no			LIKE st40_track_store_pulling.doc_no,
								whs				LIKE st40_track_store_pulling.whs,
								doc_type		LIKE st40_track_store_pulling.doc_type,
								customer		LIKE sa25_inv_hd.dl_name,
								del_add_1		LIKE sa25_inv_hd.del_add_1,
								del_add_2		LIKE sa25_inv_hd.del_add_2,
								del_add_3		LIKE sa25_inv_hd.del_add_3,
								del_add_4		LIKE sa25_inv_hd.del_add_4,
								no_of_items		LIKE st40_track_store_pulling.no_of_items,
								cut_cbl			LIKE st40_track_store_pulling.cut_cbl,
                                trip_sheet      LIKE st40_track_store_pulling.trip_sheet,
								priority_lvl	LIKE st40_track_store_pulling.priority_lvl,
								whs_phase_sort	LIKE st40_track_store_pulling.whs_phase_sort,
                                status          LIKE st40_track_store_pulling.status,
                                action_by_user  LIKE st40_track_store_pulling.action_by,
								area_code		LIKE st41_area_mast.area_code,
								area_desc		LIKE st41_area_mast.area_desc,
								route_no		LIKE st41_area_mast.route_no,
								del_order		LIKE st40_track_store_pulling.del_order
							END RECORD

	DEFINE	p_err_src							STRING,

			l_upd_str							STRING,

			lb_max_row_id						LIKE st40_track_store_pulling.row_id,
			lb_controller						LIKE st40_track_store_pulling.controller,
			lb_action_by						LIKE st40_track_store_pulling.action_by,
			lb_status							LIKE st40_track_store_pulling.status,
			lb_priority_lvl						LIKE st40_track_store_pulling.priority_lvl,

			l_status							LIKE st40_track_store_pulling.status,
			l_whs_phase_sort					LIKE st40_track_store_pulling.whs_phase_sort,
			l_controller						LIKE st40_track_store_pulling.controller,
			l_updated							BOOLEAN

	LET p_err_src = p_err_src , " > upd_doc_enq"

	IF ( pr_doc_st40.doc_no IS NULL ) THEN
		RETURN
	END IF
	
{
	Ensure controller's are transferred when moving tasks between priorities
	1. Controller = "dispatchct" and Priority = 1/2 moved to Priority 3/4
	>> Change Controller to "transctrl"

	2. Controller = "transctrl" and Priority = 3/4 moved to Priority 1/2
	>> Change Controller to "dispatchct"
}
	LET lb_max_row_id		= lf_get_field_value(	{tbl_name}		"st40_track_store_pulling",
													{field_name}	"MAX(row_id)",
													{where_clause}	"doc_type"		||" = '"|| pr_doc_st40.doc_type	||"'"	||" AND "||
																	"doc_no"		||" = '"|| pr_doc_st40.doc_no	||"'",
													{p_err_src}		p_err_src )
																
	LET lb_controller		= lf_get_field_value(	{tbl_name}		"st40_track_store_pulling",
													{fieldname}		"controller",
													{where_clause}	"doc_type"		||" = '"|| pr_doc_st40.doc_type	||"'"||" AND "||
																	"doc_no"		||" = '"|| pr_doc_st40.doc_no	||"'"||" AND "||
																	"row_id"		||" = '"|| lb_max_row_id		||"'",
													{err_src}		p_err_src )

	LET lb_action_by		= lf_get_field_value(	{tbl_name}		"st40_track_store_pulling",
													{fieldname}		"action_by",
													{where_clause}	"doc_type"		||" = '"|| pr_doc_st40.doc_type	||"'"||" AND "||
																	"doc_no"		||" = '"|| pr_doc_st40.doc_no	||"'"||" AND "||
																	"row_id"		||" = '"|| lb_max_row_id		||"'",
													{err_src}		p_err_src )

	LET lb_status			= lf_get_field_value(	{tbl_name}		"st40_track_store_pulling",
													{fieldname}		"status",
													{where_clause}	"doc_type"		||" = '"|| pr_doc_st40.doc_type	||"'"||" AND "||
																	"doc_no"		||" = '"|| pr_doc_st40.doc_no	||"'"||" AND "||
																	"row_id"		||" = '"|| lb_max_row_id		||"'",
													{err_src}		p_err_src )												

	LET lb_priority_lvl		= lf_get_field_value(	{tbl_name}		"st40_track_store_pulling",
													{fieldname}		"FIRST 1 priority_lvl",
													{where_clause}	"doc_type"		||" = '"|| pr_doc_st40.doc_type	||"'"||" AND "||
																	"doc_no"		||" = '"|| pr_doc_st40.doc_no	||"'",
													{err_src}		p_err_src )


--Determine the specific status, controller and whs_phase_sort

--1) Check if the priority level for the current whs_phase is in the new priority level
	IF ( lf_row_exists( "st40u_warehouse_user", "user_name = '"|| lb_action_by ||"' AND priority_level LIKE '%"|| lb_priority_lvl ||"%' AND priority_level LIKE '%"|| pr_doc_st40.priority_lvl ||"%'", p_err_src ) ) THEN
		LET l_updated			= TRUE
		LET l_status			= lb_status
		LET l_controller 		= lb_action_by
		LET l_whs_phase_sort	= pr_doc_st40.whs_phase_sort
	END IF
--2) If the whs_phase_sort do not have the same priority
	IF ( l_updated = FALSE ) THEN
	--Retrive the last availble status for the new priority
		LET l_status			= lf_get_field_value(	{tbl_name}		"st40u_warehouse_user",
														{fieldname}		"FIRST 1 whs_status",
														{where_clause}	"priority_level"		||" LIKE '%"|| pr_doc_st40.priority_lvl		||"%'"||" AND "||
																		"whs_phase_sort"		||" < '"|| pr_doc_st40.whs_phase_sort		||"'"||
																		"ORDER BY whs_phase_sort DESC",
														{err_src}		p_err_src )

		LET l_controller 		= lf_get_field_value(	{tbl_name}		"st40u_warehouse_user",
														{fieldname}		"FIRST 1 user_name",
														{where_clause}	"priority_level"		||" LIKE '%"|| pr_doc_st40.priority_lvl		||"%'"||" AND "||
																		"whs_phase_sort"		||" < '"|| pr_doc_st40.whs_phase_sort		||"'"||
																		"ORDER BY whs_phase_sort DESC",
														{err_src}		p_err_src )

		LET l_whs_phase_sort	= lf_get_field_value(	{tbl_name}		"st40u_warehouse_user",
														{fieldname}		"FIRST 1 whs_phase_sort",
														{where_clause}	"priority_level"		||" LIKE '%"|| pr_doc_st40.priority_lvl		||"%'"||" AND "||
																		"whs_phase_sort"		||" < '"|| pr_doc_st40.whs_phase_sort		||"'"||
																		"ORDER BY whs_phase_sort DESC",
														{err_src}		p_err_src )
	END IF

	IF ( l_status IS NOT NULL ) AND ( l_controller IS NOT NULL ) THEN
		EXECUTE IMMEDIATE	"UPDATE "													||
									"st40_track_store_pulling "							||
								"SET "													||
									"status"		||" = '"|| l_status					||"', "	||
									"whs_phase_sort"||" = '"|| l_whs_phase_sort			||"', "	||
									"controller"	||" = '"|| l_controller				||"', "	||
									"action_by"		||" = '"|| l_controller				||"' "	||
							"WHERE "				||
								"doc_type"			||" = '"|| pr_doc_st40.doc_type		||"'"	||" AND "||
								"doc_no"			||" = '"|| pr_doc_st40.doc_no		||"'"	||" AND "||
								"row_id"			||" = "	|| lb_max_row_id
	END IF

--Update priority and transport info
	LET l_upd_str = "UPDATE "								||
							"st40_track_store_pulling "		||
						"SET "

	IF ( pr_doc_st40.area_code IS NULL ) THEN
		LET l_upd_str = l_upd_str ||
								"area_code"	||" = NULL ,"
	ELSE
		LET l_upd_str = l_upd_str ||
								"area_code"	||" = '"|| pr_doc_st40.area_code	||"', "
	END IF

	IF ( pr_doc_st40.route_no IS NULL ) THEN
		LET l_upd_str = l_upd_str ||
								"route_no"	||" = NULL ,"
	ELSE
		LET l_upd_str = l_upd_str ||
								"route_no"	||" = '"|| pr_doc_st40.route_no	||"', "
	END IF

	IF ( pr_doc_st40.del_order IS NULL ) THEN
		LET l_upd_str = l_upd_str ||
								"del_order"	||" = NULL ,"
	ELSE
		LET l_upd_str = l_upd_str ||
								"del_order"	||" = "|| pr_doc_st40.del_order	||", "
	END IF

	LET l_upd_str = l_upd_str ||
							"priority_lvl"	||" = "	|| pr_doc_st40.priority_lvl	||" "||
					"WHERE "				||
							"doc_type"		||" = '"|| pr_doc_st40.doc_type		||"'"||" AND "||
							"doc_no"		||" = '"|| pr_doc_st40.doc_no		||"'"
							
	EXECUTE IMMEDIATE l_upd_str

END FUNCTION


{==================================================================================================================================}
#+ UPDATE FUNCTION FOR ACTION LOGS
#+
#+ BUSINESS RULE:
#+ Updates the DB for actions logged
#+
#+
#+ @code CALL upd_log_action( p_err_src )
#+
#+ @param p_err_src     Error tracking variable
#+
#+ @return NONE
#+
#+ CHANGES
#+
FUNCTION upd_log_action( p_err_src )

    DEFINE	p_err_src				    STRING,
			l_cnt					    INTEGER,

			l_last_phase			    BOOLEAN,
            l_force_assign_user_action  BOOLEAN,
            l_insert_st40_entry         BOOLEAN,
            l_last_phase_sort           LIKE st40_track_store_pulling.whs_phase_sort,
            l_next_phase_sort           LIKE st40_track_store_pulling.whs_phase_sort,
            l_filter                    STRING,
            l_doc_no                    VARCHAR(14)

	DEFINE	lr_st40					    RECORD LIKE st40_track_store_pulling.*

	DEFINE 	la_st40u				    DYNAMIC ARRAY OF RECORD	 --Used to store all the phases for the priority
                                            user_name		LIKE st40u_warehouse_user.user_name,
                                            whs_phase_user	LIKE st40u_warehouse_user.whs_phase_user,
                                            whs_phase_sort	LIKE st40u_warehouse_user.whs_phase_sort,
                                            trip_plan	    LIKE st40u_warehouse_user.trip_plan,
                                            trip_return	    LIKE st40u_warehouse_user.trip_return,
                                            final_phase     LIKE st40u_warehouse_user.final_phase,
                                            whs_status		LIKE st40u_warehouse_user.whs_status
                                        END RECORD

    LET p_err_src = p_err_src , " > upd_log_action"

   -- Add filters based off what the current document status is
    IF ( lf_row_exists( "st40u_warehouse_user", "type = 'R' AND	whs_phase_user = 'Y' AND user_name = '"||mr_st40_action.user_name||"' AND po_hold = 'Y'", "sb_cbox_build_doc_enq" ) ) THEN 
        LET l_filter = "AND (po_hold = 'Y' OR final_phase = 'Y')"
    END IF 

    IF ( lf_row_exists( "st40u_warehouse_user", "type = 'R' AND	whs_phase_user = 'Y' AND user_name = '"||mr_st40_action.user_name||"' and cd_hold = 'Y'", "sb_cbox_build_doc_enq" ) ) THEN 
        LET l_filter = "AND (cd_hold = 'Y' OR final_phase = 'Y')"
    END IF 

    IF ( l_filter IS NULL ) THEN 
        LET l_filter = "AND po_hold = 'N' AND cd_hold = 'N'" 
    END IF    
    
--Retrive the list of whs phases for the priority specified
	PREPARE qry_st40u_log FROM "SELECT user_name, whs_phase_user, whs_phase_sort, trip_plan, trip_return,final_phase, whs_status FROM st40u_warehouse_user WHERE ( whs_phase_user = 'Y' OR user_name = '"||mr_st40_action.user_name||"') AND type = 'R' "||l_filter||" ORDER BY whs_phase_sort"								
	DECLARE curs_st40u_log CURSOR FOR qry_st40u_log

	LET l_cnt = 1
	
	FOREACH curs_st40u_log INTO la_st40u[l_cnt].*
		LET l_cnt = l_cnt + 1
	END FOREACH

	CALL la_st40u.deleteElement(l_cnt)

    IF ( mr_st40_action.doc_type = "IBT" ) THEN 
        LET l_last_phase_sort 	= lf_get_field_value( 	{tbl_name}		"st40_track_store_pulling", 
                                                        {field_name}	"FIRST 1(whs_phase_sort)", 
                                                        {where_clause}	"doc_type = '" || mr_st40_action.doc_type || "' AND priority_lvl = 5 AND doc_no = '" || mr_st40_action.doc_no||","||mr_st40_action.ship_doc_no || "' ORDER BY row_id DESC", 
                                                        p_err_src )

        LET l_next_phase_sort   = lf_get_field_value( 	{tbl_name}		"st40u_warehouse_user", 
                                                        {field_name}	"FIRST 1(whs_phase_sort)", 
                                                        {where_clause}	"whs_phase_sort > '" || l_last_phase_sort || "' "||l_filter||" AND type = 'R' ORDER BY whs_phase_sort asc", 
                                                        p_err_src )

    ELSE 
        LET l_last_phase_sort 	= lf_get_field_value( 	{tbl_name}		"st40_track_store_pulling", 
                                                        {field_name}	"FIRST 1(whs_phase_sort)", 
                                                        {where_clause}	"doc_type = '" || mr_st40_action.doc_type || "' AND priority_lvl = 5 AND doc_no = '" || mr_st40_action.doc_no || "' ORDER BY row_id DESC", 
                                                        p_err_src )

        LET l_next_phase_sort   = lf_get_field_value( 	{tbl_name}		"st40u_warehouse_user", 
                                                        {field_name}	"FIRST 1(whs_phase_sort)", 
                                                        {where_clause}	"whs_phase_sort > '" || l_last_phase_sort || "' "||l_filter||" AND type = 'R' ORDER BY whs_phase_sort asc", 
                                                        p_err_src )
    END IF 
    
--Set flags
    LET l_force_assign_user_action  = FALSE 
    LET l_insert_st40_entry         = TRUE 
	LET l_last_phase		 	    = TRUE

	IF sb_task_exists( p_err_src ) THEN
        IF ( mr_st40_action.doc_type = "IBT" ) THEN 	
            LET l_doc_no = mr_st40_action.doc_no ||","||mr_st40_action.ship_doc_no
        ELSE 
            LET l_doc_no = mr_st40_action.doc_no 
        END IF 
        
        EXECUTE IMMEDIATE	"UPDATE "								||
                                    "st40_track_store_pulling "		||
                                "SET "								||
                                    "end_date"	||" = "	|| "TODAY"	||", "||
                                    "end_time"	||" = '"|| TIME		||"' "||
                            "WHERE "			||
                                    "doc_type"	||" = '"|| mr_st40_action.doc_type	||"'"||" AND "||
                                    "doc_no"	||" = '"|| l_doc_no	                ||"'"||" AND "||
                                    "action_by"	||" = '"|| mr_st40_action.user_name	||"'"


    --Default the End Date and Time to NULL
        LET lr_st40.end_date		= NULL
        LET lr_st40.end_time		= NULL

    --Loop through the phases to retrive the next value	phase -- If its the final phase then do NOT continue
        FOR l_cnt = 1 TO la_st40u.getLength() 
            IF ( l_next_phase_sort = la_st40u[l_cnt].whs_phase_sort ) AND ( la_st40u[l_cnt].final_phase = 'N') THEN
                LET l_last_phase			= FALSE
                LET lr_st40.status 			= la_st40u[l_cnt].whs_status
                LET lr_st40.controller		= la_st40u[l_cnt].user_name
                LET lr_st40.action_by		= la_st40u[l_cnt].user_name
                LET lr_st40.whs_phase_sort	= la_st40u[l_cnt].whs_phase_sort

                EXIT FOR 
            END IF
        END FOR
        
    --Use the last phase of the priority
        IF ( l_last_phase = TRUE ) THEN
            LET lr_st40.status 			= la_st40u[la_st40u.getLength()].whs_status
            LET lr_st40.controller		= la_st40u[la_st40u.getLength()].user_name
            LET lr_st40.action_by		= la_st40u[la_st40u.getLength()].user_name
            LET lr_st40.whs_phase_sort	= la_st40u[la_st40u.getLength()].whs_phase_sort
            LET lr_st40.end_date		= TODAY
            LET lr_st40.end_time		= TIME

        END IF

    --Bin Update
{			IF ( mr_st40_action.next_action = 1 ) THEN
            CALL upd_bin_qty ( mr_st40_org.doc_type, mr_st40_org.doc_no, mr_st40_org.loc, mr_st40_org.whs, p_err_src )
        END IF}
        
    -- Check if the curren phase is is force_asign_user = 'Y' phase
        IF ( lf_row_exists(	"st40u_warehouse_user", "whs_phase_user = 'Y' AND whs_phase_sort = " || l_last_phase_sort ||" AND force_assign_user = 'Y' ", p_err_src ) )THEN 
            LET l_force_assign_user_action = TRUE 
        END IF 
        
    -- If it is a force_assign_user phase = 'Y' then make sure, that we only move to the next phase when there is no more NON-PHASE users st40 uncompleted enteries
        IF ( sb_force_assign_user_true( FALSE, l_last_phase_sort, mr_st40_org.loc, mr_st40_org.whs, mr_st40_org.doc_type, mr_st40_org.doc_no, p_err_src ) ) THEN  
            -- Make sure that we allow new users to action the st40 entries -> we do this by checking if the current actioned user has a st40 entry 
            -- already for the document
            IF ( lf_row_count( "st40_track_store_pulling", "doc_type = '"|| mr_st40_action.doc_type||"' AND doc_no = '"|| l_doc_no ||"' AND end_date IS NULL AND end_time IS NULL", p_err_src ) > 1 ) THEN 
                LET l_insert_st40_entry = TRUE  
            ELSE 
                LET l_insert_st40_entry = FALSE 
            END IF 
        END IF

    --Create New Task
        LET lr_st40.loc				= mr_st40_org.loc
        LET lr_st40.whs				= mr_st40_org.whs
        LET lr_st40.doc_type		= mr_st40_org.doc_type
        LET lr_st40.doc_no			= l_doc_no
        LET lr_st40.row_id			= lf_get_field_value(	{tbl_name}		"st40_track_store_pulling",
                                                            {field_name}	"MAX(row_id)",
                                                            {where_clause}	"doc_type"	||" = '"|| mr_st40_action.doc_type	||"'"	||" AND "||
                                                                            "doc_no"	||" = '"|| l_doc_no	||"'",
                                                            {p_err_src}		p_err_src ) + 1	--Increment the MAX row id by 1

        LET lr_st40.priority_lvl	= mr_st40_org.priority_lvl
        LET lr_st40.collect_req_by	= mr_st40_org.collect_req_by

        LET lr_st40.dest_name		= mr_st40_org.dest_name
        LET lr_st40.dest_add_1		= mr_st40_org.dest_add_1
        LET lr_st40.dest_add_2		= mr_st40_org.dest_add_2
        LET lr_st40.dest_add_3		= mr_st40_org.dest_add_3
        LET lr_st40.dest_add_4		= mr_st40_org.dest_add_4

        LET lr_st40.exported		= mr_st40_org.exported
        LET lr_st40.exported_date	= mr_st40_org.exported_date
        LET lr_st40.exported_time	= mr_st40_org.exported_time
        
        LET lr_st40.start_date		= TODAY
        LET lr_st40.start_time		= TIME
    --End Date and Time set above the CASE statement
        LET lr_st40.no_of_items		= mr_st40_org.no_of_items
        LET lr_st40.cut_cbl			= mr_st40_org.cut_cbl
        LET lr_st40.area_code		= mr_st40_org.area_code
        LET lr_st40.trip_sheet		= mr_st40_org.trip_sheet
        LET lr_st40.route_no		= mr_st40_org.route_no
        LET lr_st40.del_order		= mr_st40_org.del_order

        TRY
            IF ( l_insert_st40_entry ) THEN 
                INSERT INTO st40_track_store_pulling VALUES( lr_st40.* )
            END IF 
            
        -- make sure to update the original warehouse st40 entry ( the one before the force_assign_user = 'Y' phase ) when completing all NON user phases
            IF ( l_force_assign_user_action ) AND ( l_insert_st40_entry ) THEN
                EXECUTE IMMEDIATE	"UPDATE "							    	||
                                        "st40_track_store_pulling "		        ||
                                    "SET "								        ||
                                        "end_date"	        ||" = "	|| "TODAY"	||", "||
                                        "end_time"	        ||" = '"|| TIME		||"' "||
                                    "WHERE "			    ||
                                        "doc_type"	        ||" = '"|| mr_st40_action.doc_type	||"'"||" AND "||
                                        "doc_no"	        ||" = '"|| l_doc_no	                ||"'"||" AND "||
                                        "whs_phase_sort"	||" =  "|| l_last_phase_sort
            END IF 

            
        CATCH
            LET p_err_src = lf_sql_error( p_err_src, "Failure to create Warehouse Tracking Record!" )
            CALL fgl_winmessage( %"Error", p_err_src, "stop" )
            RETURN
        END TRY
		
	ELSE
	--Create New Task
		LET lr_st40.loc				= mr_st40_org.loc
		LET lr_st40.whs				= mr_st40_org.whs
		LET lr_st40.doc_type		= mr_st40_action.doc_type
        
        IF ( mr_st40_action.doc_type = "IBT" ) THEN 	
            LET lr_st40.doc_no			= mr_st40_action.doc_no||","||mr_st40_action.ship_doc_no
            LET lr_st40.row_id			= lf_get_field_value(	{tbl_name}		"st40_track_store_pulling",
                                                                {field_name}	"MAX(row_id)",
                                                                {where_clause}	"doc_type"	||" = '"|| mr_st40_action.doc_type	||"'"	||" AND "||
                                                                                "doc_no"	||" = '"|| mr_st40_action.doc_no||","||mr_st40_action.ship_doc_no	||"'",
                                                                {p_err_src}		p_err_src ) + 1	--Increment the MAX row id by 1
        ELSE 
            LET lr_st40.doc_no			= mr_st40_org.doc_no
            LET lr_st40.row_id			= lf_get_field_value(	{tbl_name}		"st40_track_store_pulling",
                                                                {field_name}	"MAX(row_id)",
                                                                {where_clause}	"doc_type"	||" = '"|| mr_st40_action.doc_type	||"'"	||" AND "||
                                                                                "doc_no"	||" = '"|| mr_st40_action.doc_no	||"'",
                                                                {p_err_src}		p_err_src ) + 1	--Increment the MAX row id by 1
        END IF 
        
		LET lr_st40.whs_phase_sort	= mr_st40_org.whs_phase_sort
		LET lr_st40.priority_lvl	= mr_st40_org.priority_lvl
		LET lr_st40.collect_req_by	= mr_st40_org.collect_req_by
		LET lr_st40.dest_name		= mr_st40_org.dest_name
		LET lr_st40.dest_add_1		= mr_st40_org.dest_add_1
		LET lr_st40.dest_add_2		= mr_st40_org.dest_add_2
		LET lr_st40.dest_add_3		= mr_st40_org.dest_add_3
		LET lr_st40.dest_add_4		= mr_st40_org.dest_add_4
        
		LET lr_st40.exported		= mr_st40_org.exported
		LET lr_st40.exported_date	= mr_st40_org.exported_date
		LET lr_st40.exported_time	= mr_st40_org.exported_time
		LET lr_st40.start_date		= TODAY
		LET lr_st40.start_time		= TIME
		LET lr_st40.end_date		= NULL
		LET lr_st40.end_time		= NULL

		LET lr_st40.action_by		= mr_st40_action.user_name
		LET lr_st40.no_of_items		= mr_st40_org.no_of_items
		LET lr_st40.cut_cbl			= mr_st40_org.cut_cbl
		LET lr_st40.area_code		= mr_st40_org.area_code
		LET lr_st40.trip_sheet		= mr_st40_org.trip_sheet
		LET lr_st40.route_no		= mr_st40_org.route_no

        -- Check if the new phase is Force assing user for the Priority Level - If it is then make sure that we insert the new element
        -- For the force assing user aswell as updating ALL current st40 enteriess ( there shouldn't be more than 1 st40 entry at this point )
        IF ( lf_row_exists( "st40u_warehouse_user","whs_phase_user = 'Y' AND type = 'R' AND force_assign_user = 'Y' AND priority_level LIKE '%"||lr_st40.priority_lvl||"%' AND whs_phase_sort = "||l_next_phase_sort, p_err_src ) ) THEN 

            -- Update current document status then get all details for the force assign user for the priority
            IF ( mr_st40_action.doc_type = "IBT" ) THEN 
                EXECUTE IMMEDIATE	    "UPDATE "							||
                                            "st40_track_store_pulling "		||
                                        "SET "								||
                                            "end_date"	||" = "	|| "TODAY"	||", "||
                                            "end_time"	||" = '"|| TIME		||"' "||
                                        "WHERE "		||
                                            "doc_type"	||" = '"|| mr_st40_action.doc_type	||"'"||" AND "||
                                            "doc_no"	||" = '"|| mr_st40_action.doc_no||","||mr_st40_action.ship_doc_no	||"'"
            ELSE 
                EXECUTE IMMEDIATE	    "UPDATE "							||
                                            "st40_track_store_pulling "		||
                                        "SET "								||
                                            "end_date"	||" = "	|| "TODAY"	||", "||
                                            "end_time"	||" = '"|| TIME		||"' "||
                                        "WHERE "		||
                                            "doc_type"	||" = '"|| mr_st40_action.doc_type	||"'"||" AND "||
                                            "doc_no"	||" = '"|| mr_st40_action.doc_no	||"'"
            END IF 
            
            LET lr_st40.controller		= lf_get_field_value( 	{tbl_name}		"st40u_warehouse_user",
                                                                {field_name}	"user_name",
                                                                {where_clause}	"type = 'R' AND force_assign_user = 'Y' AND priority_level LIKE '%"||lr_st40.priority_lvl||"%' ",
                                                                {p_err_src}		p_err_src )

            LET lr_st40.whs_phase_sort = l_next_phase_sort

            -- Status based off the WHS force assign user
            LET lr_st40.status			= lf_get_field_value( 	{tbl_name}		"st40u_warehouse_user", 
                                                                {field_name}	"whs_status", 
                                                                {where_clause}	"type = 'R' AND force_assign_user = 'Y' AND priority_level LIKE '%"||lr_st40.priority_lvl||"%' ",
                                                                {p_err_src}		p_err_src)  
                                                                
        ELSE 
            LET lr_st40.controller		= lf_get_field_value( 	{tbl_name}		"st40u_warehouse_user",
                                                                {field_name}	"user_name",
                                                                {where_clause}	"whs_phase_sort = '0' AND type = 'R'",
                                                                {p_err_src}		p_err_src )
                                                                
            LET lr_st40.whs_phase_sort  = lf_get_field_value ( 	{tbl_name}		"st40_track_store_pulling", 
                                                                {field_name}	"FIRST 1(whs_phase_sort)", 
                                                                {where_clause}	"doc_no = '"	|| lr_st40.doc_no       ||"' AND "|| 
                                                                                "doc_type = '"	|| lr_st40.doc_type 	||"' AND "||
                                                                                "end_date IS NULL ORDER BY whs_phase_sort asc", 
                                                                {p_err_src}		p_err_src ) 

            -- Status based off the WHS Phase user
            LET lr_st40.status			= lf_get_field_value( 	{tbl_name}		"st40u_warehouse_user", 
                                                                {field_name}	"whs_status", 
                                                                {where_clause}	"whs_phase_user = 'Y' AND whs_phase_sort = '" ||lr_st40.whs_phase_sort||"' AND type = 'R'", 
                                                                {p_err_src}		p_err_src)
        END IF 
        
		TRY
			INSERT INTO st40_track_store_pulling VALUES( lr_st40.* )
		CATCH
			LET p_err_src = lf_sql_error( p_err_src, "Failure to create Warehouse Tracking Record!" )
			CALL fgl_winmessage( %"Error", p_err_src, "stop" )
			RETURN
		END TRY
	END IF


END FUNCTION

{==================================================================================================================================}
#+ UPDATE FUNCTION FOR ACTION LOGS
#+
#+ BUSINESS RULE:
#+ Updates the DB for actions logged
#+
#+
#+ @code CALL upd_status( pr_st40_org, pr_doc_st40, p_err_src )
#+
#+ @param pr_st40_org       Original st40 
#+ @param pr_doc_st40       Data comes from doc Enq input
#+ @param p_err_src         Error tracking variable
#+
#+ @return NONE
#+
#+ CHANGES
#+
FUNCTION upd_status( pr_st40_org, pr_doc_st40, p_err_src ) 
 
	DEFINE  pr_st40_org                 RECORD LIKE st40_track_store_pulling.*,
                pr_doc_st40		        RECORD
                                            doc_no			LIKE st40_track_store_pulling.doc_no,
                                            whs				LIKE st40_track_store_pulling.whs,
                                            doc_type		LIKE st40_track_store_pulling.doc_type,
                                            customer		LIKE sa25_inv_hd.dl_name,
                                            del_add_1		LIKE sa25_inv_hd.del_add_1,
                                            del_add_2		LIKE sa25_inv_hd.del_add_2,
                                            del_add_3		LIKE sa25_inv_hd.del_add_3,
                                            del_add_4		LIKE sa25_inv_hd.del_add_4,
                                            no_of_items		LIKE st40_track_store_pulling.no_of_items,
                                            cut_cbl			LIKE st40_track_store_pulling.cut_cbl,
                                            trip_sheet      LIKE st40_track_store_pulling.trip_sheet,
                                            priority_lvl	LIKE st40_track_store_pulling.priority_lvl,
                                            whs_phase_sort	LIKE st40_track_store_pulling.whs_phase_sort,
                                            status          LIKE st40_track_store_pulling.status,
                                            action_by_user  LIKE st40_track_store_pulling.action_by,
                                            area_code		LIKE st41_area_mast.area_code,
                                            area_desc		LIKE st41_area_mast.area_desc,
                                            route_no		LIKE st41_area_mast.route_no,
                                            del_order		LIKE st40_track_store_pulling.del_order
                                        END RECORD,
            p_err_src				    STRING,
			l_cnt					    INTEGER,
            l_last_phase                BOOLEAN,
            l_cur_whs_phase_sort_no     LIKE st40_track_store_pulling.whs_phase_sort,
            l_filter                    STRING 

	DEFINE	lr_st40					    RECORD LIKE st40_track_store_pulling.*
	DEFINE 	la_st40u				    DYNAMIC ARRAY OF RECORD	 --Used to store all the phases for the priority
                                            user_name		LIKE st40u_warehouse_user.user_name,
                                            whs_phase_user	LIKE st40u_warehouse_user.whs_phase_user,
                                            whs_phase_sort	LIKE st40u_warehouse_user.whs_phase_sort,
                                            trip_plan	    LIKE st40u_warehouse_user.trip_plan,
                                            trip_return	    LIKE st40u_warehouse_user.trip_return,
                                            final_phase     LIKE st40u_warehouse_user.final_phase,
                                            whs_status		LIKE st40u_warehouse_user.whs_status
                                        END RECORD

    LET p_err_src = p_err_src , " > upd_status"


    -- Add filters based off what the current document status is
    IF ( lf_row_exists( "st40u_warehouse_user","type = 'R' AND	whs_phase_user = 'Y' AND whs_status = '"||pr_st40_org.status||"' AND po_hold = 'Y'", "sb_cbox_build_doc_enq" ) ) THEN 
        LET l_filter = "AND (po_hold = 'Y' OR final_phase = 'Y')"
    END IF 

    IF ( lf_row_exists( "st40u_warehouse_user","type = 'R' AND	whs_phase_user = 'Y' AND whs_status = '"||pr_st40_org.status||"' and cd_hold = 'Y'", "sb_cbox_build_doc_enq" ) ) THEN 
        LET l_filter = "AND (cd_hold = 'Y' OR final_phase = 'Y')"
    END IF 

    IF ( l_filter IS NULL ) THEN 
        LET l_filter = "AND po_hold = 'N' AND cd_hold = 'N'" 
    END IF 
    
 --Retrive the list of whs phases for the priority specified
	PREPARE qry_st40u_upd_status FROM "SELECT user_name, whs_phase_user, whs_phase_sort, trip_plan, trip_return, final_phase, whs_status FROM st40u_warehouse_user WHERE ( whs_phase_user = 'Y' OR whs_status = '"||pr_doc_st40.status||"') AND type = 'R' "||l_filter||" ORDER BY whs_phase_sort"								
	DECLARE curs_st40u_upd_status CURSOR FOR qry_st40u_upd_status

	LET l_cnt = 1
	
	FOREACH curs_st40u_upd_status INTO la_st40u[l_cnt].*
		LET l_cnt = l_cnt + 1
	END FOREACH

	CALL la_st40u.deleteElement(l_cnt)

    -- Define Starting Variables:
    LET l_last_phase = TRUE 

    -- Get the whs phase sort position
    LET l_cur_whs_phase_sort_no = lf_get_field_value ( "st40u_warehouse_user","whs_phase_sort","whs_status = '"||pr_doc_st40.status||"' AND whs_phase_user = 'Y' AND type = 'R' "||l_filter, p_err_src )
      
    EXECUTE IMMEDIATE	"UPDATE "								||
                                "st40_track_store_pulling "		||
                            "SET "								||
                                "end_date"	||" = "	|| "TODAY"	||", "||
                                "end_time"	||" = '"|| TIME		||"' "||
                        "WHERE "			||
                                "doc_type"	||" = '"|| pr_doc_st40.doc_type	||"'"||" AND "||
                                "doc_no"	||" = '"|| pr_doc_st40.doc_no	||"'"

    -- Set to Null to Start
    LET lr_st40.end_date		= NULL 
    LET lr_st40.end_time		= NULL
    
    --Loop through the phases to retrive the next value	phase
    FOR l_cnt = 1 TO la_st40u.getLength()
        IF ( l_cur_whs_phase_sort_no = la_st40u[l_cnt].whs_phase_sort ) AND ( la_st40u[l_cnt].final_phase = 'N') THEN
            LET l_last_phase			= FALSE
            LET lr_st40.status 			= la_st40u[l_cnt].whs_status
            LET lr_st40.controller		= la_st40u[l_cnt].user_name
            LET lr_st40.action_by		= la_st40u[l_cnt].user_name
            LET lr_st40.whs_phase_sort	= la_st40u[l_cnt].whs_phase_sort
        END IF
    END FOR

--Use the last phase of the priority
    IF ( l_last_phase = TRUE ) THEN
        LET lr_st40.status 			= la_st40u[la_st40u.getLength()].whs_status
        LET lr_st40.controller		= la_st40u[la_st40u.getLength()].user_name
        LET lr_st40.action_by		= la_st40u[la_st40u.getLength()].user_name
        LET lr_st40.whs_phase_sort	= la_st40u[la_st40u.getLength()].whs_phase_sort
        LET lr_st40.end_date		= TODAY
        LET lr_st40.end_time		= TIME


    END IF
	--Create New Task
    LET lr_st40.loc				= pr_st40_org.loc
    LET lr_st40.whs				= pr_st40_org.whs
    LET lr_st40.doc_type		= pr_st40_org.doc_type
    LET lr_st40.doc_no			= pr_st40_org.doc_no
    LET lr_st40.row_id			= lf_get_field_value(	{tbl_name}		"st40_track_store_pulling",
                                                        {field_name}	"MAX(row_id)",
                                                        {where_clause}	"doc_type"	||" = '"|| pr_st40_org.doc_type	||"'"	||" AND "||
                                                                        "doc_no"	||" = '"|| pr_st40_org.doc_no	||"'",
                                                        {p_err_src}		p_err_src ) + 1	--Increment the MAX row id by 1

    LET lr_st40.whs_phase_sort	= pr_st40_org.whs_phase_sort
    LET lr_st40.priority_lvl	= pr_doc_st40.priority_lvl
    LET lr_st40.collect_req_by	= pr_st40_org.collect_req_by
    LET lr_st40.dest_name		= pr_st40_org.dest_name
    LET lr_st40.dest_add_1		= pr_st40_org.dest_add_1
    LET lr_st40.dest_add_2		= pr_st40_org.dest_add_2
    LET lr_st40.dest_add_3		= pr_st40_org.dest_add_3
    LET lr_st40.dest_add_4		= pr_st40_org.dest_add_4
    LET lr_st40.status			= pr_doc_st40.status
    LET lr_st40.exported		= pr_st40_org.exported
    LET lr_st40.exported_date	= pr_st40_org.exported_date
    LET lr_st40.exported_time	= pr_st40_org.exported_time
    LET lr_st40.start_date		= TODAY
    LET lr_st40.start_time		= TIME

    -- If the status has been changes to the first whs phase then set the controller and action by to default
    IF ( lr_st40.status = lf_get_field_value( "st40u_warehouse_user","whs_status","whs_phase_sort = 0 AND type = 'R'", p_err_src ) {OR ( lr_st40.status = gr_st00.whs_default_status ) }) THEN 
        LET lr_st40.controller		= lf_get_field_value( "st40u_warehouse_user","user_name","whs_phase_sort = 0 AND type = 'R'", 		p_err_src )
        LET lr_st40.action_by		= lf_get_field_value( "st40u_warehouse_user","user_name","whs_phase_sort = 0 AND type = 'R'", 		p_err_src )
    ELSE 
        LET lr_st40.controller		= lf_get_field_value( "st40u_warehouse_user","user_name","whs_status = '"||lr_st40.status||"' AND type = 'R'", 		p_err_src )

        -- Check if the current status is force assing user = 'Y' > If it is then set the action by to the user selected 
        -- In the doc enq
        IF ( lf_row_exists( "st40u_warehouse_user","type = 'R' AND force_assign_user = 'Y' AND priority_level LIKE '%"||pr_doc_st40.priority_lvl||"%' AND whs_phase_user = 'Y' AND whs_status = '"||lr_st40.status||"'", p_err_src ) ) THEN 
            LET lr_st40.action_by	= pr_doc_st40.action_by_user
        ELSE 
            LET lr_st40.action_by	= lf_get_field_value( "st40u_warehouse_user","user_name","whs_status = '"||lr_st40.status||"' AND type = 'R'", 		p_err_src )
        END IF 
        
    END IF 
    
    LET lr_st40.no_of_items		= pr_st40_org.no_of_items
    LET lr_st40.cut_cbl			= pr_st40_org.cut_cbl
    LET lr_st40.area_code		= pr_st40_org.area_code
    LET lr_st40.trip_sheet		= pr_st40_org.trip_sheet
    LET lr_st40.route_no		= pr_st40_org.route_no
    
    -- Check if the current status is force assing user = 'Y' > If it is then set the whs phase sort of the force assign user's
    -- phase pos
    IF ( lf_row_exists( "st40u_warehouse_user","type = 'R' AND force_assign_user = 'Y' AND priority_level LIKE '%"||pr_doc_st40.priority_lvl||"%' AND whs_phase_user = 'Y' AND whs_status = '"||lr_st40.status||"'", p_err_src ) ) THEN 
        LET lr_st40.whs_phase_sort = lf_get_field_value ( 	{tbl_name}		"st40u_warehouse_user", 
                                                            {field_name}	"whs_phase_sort", 
                                                            {where_clause}	"type = 'R' AND force_assign_user = 'Y' AND priority_level LIKE '%"||pr_doc_st40.priority_lvl||"%' AND whs_phase_user = 'Y'", 
                                                            {p_err_src}		p_err_src )
    ELSE 
        LET lr_st40.whs_phase_sort = lf_get_field_value ( 	{tbl_name}		"st40u_warehouse_user", 
                                                            {field_name}	"whs_phase_sort", 
                                                            {where_clause}	"whs_phase_user = 'Y' AND user_name = '"|| lr_st40.action_by ||"' AND type = 'R'", 
                                                            {p_err_src}		p_err_src )
    END IF 
    
    TRY
        INSERT INTO st40_track_store_pulling VALUES( lr_st40.* )

    CATCH
        LET p_err_src = lf_sql_error( p_err_src, "Failure to create Warehouse Tracking Record!" )
        CALL fgl_winmessage( %"Error", p_err_src, "stop" )
        RETURN
    END TRY

END FUNCTION

{==================================================================================================================================}
#+ UPDATE FUNCTION FOR BIN TRACKING
#+
#+ BUSINESS RULE:
#+ Updates the DB for bin tracking
#+
#+ @code CALL upd_bin_qty ( p_doc_type, p_doc_no, p_loc, p_whs, p_err_src )
#+
#+ @param p_err_src Error tracking variable
#+
#+ @return NONE
#+
#+ CHANGES
#+

FUNCTION upd_bin_qty ( p_doc_type, p_doc_no, p_loc, p_whs, p_err_src )

	DEFINE 	p_doc_type	LIKE st40_track_store_pulling.doc_type,
			p_doc_no	LIKE st40_track_store_pulling.doc_no,
			p_loc		LIKE st40_track_store_pulling.loc,
			p_whs		LIKE st40_track_store_pulling.whs,
			p_err_src	STRING,

			l_cnt		INTEGER,
			l_ship_doc	LIKE st40_track_store_pulling.doc_no,
			l_ship_no	LIKE ib31i_bin_alloc.ship_doc_no,

			lr_st30i	RECORD LIKE st30i_internal_trans.*,

			la_string	DYNAMIC ARRAY OF STRING,

			la_stk		DYNAMIC ARRAY OF RECORD
							bin_no		LIKE st02b_loc_bins.bin_no,
							stk_code	LIKE st02b_loc_bins.stk_code,
							bin_alloc	LIKE st02b_loc_bins.bin_qty,
							row_id		LIKE st02b_loc_bins.row_id
						END RECORD,

			la_stk_ibp	DYNAMIC ARRAY OF RECORD
							bin_no		LIKE st02b_loc_bins.bin_no,
							stk_code	LIKE st02b_loc_bins.stk_code,
							bin_alloc	LIKE st02b_loc_bins.bin_qty,
							ship_no		LIKE ib31i_bin_alloc.ship_doc_no
						END RECORD			

	TRY
		BEGIN WORK

	--Check the Type
		CASE p_doc_type
		
			WHEN "INV"
			--BUILD Bin Array of stk codes to update
				PREPARE qty_inv	FROM "SELECT bin_no, stk_code, bin_qty_to_pull_pack FROM sa26i_bin_alloc WHERE doc_no = '"||p_doc_no||"'"
				DECLARE cur_inv CURSOR FOR qty_inv

				LET l_cnt = 1

				FOREACH cur_inv INTO la_stk[l_cnt].*
					LET l_cnt = l_cnt + 1
				END FOREACH

				CALL la_stk.deleteElement(l_cnt)

			--UPDATE for bin qty
				FOR l_cnt = 1 TO la_stk.getLength()
				
					IF ( la_stk[l_cnt].bin_alloc <> 0 ) THEN
					--st02b update
						CALL lf_update_st02b ( p_loc, p_whs, la_stk[l_cnt].stk_code, la_stk[l_cnt].bin_no, "bin_qty_to_pull_pack", la_stk[l_cnt].bin_alloc, "-", p_err_src ) RETURNING g_not_used_string
						CALL lf_update_st02b ( p_loc, p_whs, la_stk[l_cnt].stk_code, la_stk[l_cnt].bin_no, "bin_qty", la_stk[l_cnt].bin_alloc, "+", p_err_src ) RETURNING g_not_used_string
					--sa26i update
						EXECUTE IMMEDIATE "UPDATE sa26i_bin_alloc SET bin_qty_to_pull_pack = 0 WHERE doc_no = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"' AND bin_no = '"||la_stk[l_cnt].bin_no||"'"

					--st30i update
						LET lr_st30i.stk_code		= la_stk[l_cnt].stk_code
                        LET lr_st30i.st30_row_id    = lf_get_field_value ( "st30_stk_tran", "FIRST 1 row_id", "tran_type = 'INV' AND ref_1 = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"'", p_err_src)
                        CALL lf_increment( "st01_mast", "last_st30i_row_id", "stk_code = '"||la_stk[l_cnt].stk_code||"'", 0, 0 ) RETURNING g_not_used,  lr_st30i.row_id
                        LET lr_st30i.loc			= p_loc
                        LET lr_st30i.whs			= p_whs
                        LET lr_st30i.period			= gr_st00.period
                        LET lr_st30i.tran_type		= "INV"
                        LET lr_st30i.tran_date		= TODAY
                        LET lr_st30i.tran_time		= CURRENT HOUR TO SECOND
                        LET lr_st30i.create_by		= lf_get_field_value ( "st30_stk_tran", "FIRST 1 create_by", "tran_type = 'INV' AND ref_1 = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"'", p_err_src)
                        LET lr_st30i.batch_no		= NULL
                        LET lr_st30i.ref_1			= p_doc_no
                        LET lr_st30i.ref_2			= la_stk[l_cnt].bin_no
                        LET lr_st30i.uom			= lf_get_field_value ( "st30_stk_tran", "FIRST 1 uom", "tran_type = 'INV' AND ref_1 = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"'", p_err_src)
                        LET lr_st30i.qty			= la_stk[l_cnt].bin_alloc
                        LET lr_st30i.phy_bal		= lf_get_field_value("st02b_loc_bins","bin_qty","stk_code = '"||la_stk[l_cnt].stk_code||"' AND loc = '"||p_loc||"' AND whs = '"||p_whs||"' AND bin_no = '"||la_stk[l_cnt].bin_no||"'", p_err_src)

						IF NOT( lf_update_st30i(	{r_st30i}			base.TypeInfo.create(lr_st30i),
													{action_selected}	"create",
													{err_src}			p_err_src ) )
						THEN
							LET p_err_src = lf_sql_error ( p_err_src ||" > UPDATE st30i ", la_stk[l_cnt].stk_code )
							ROLLBACK WORK
							CALL fgl_winmessage( %"Error", p_err_src, "stop" )
							RETURN
						END IF
							
					END IF
				
				END FOR
				
			WHEN "CRN"
			--BUILD Bin Array of stk codes to update
				PREPARE qty_crn	FROM "SELECT bin_no, stk_code, bin_qty_to_pull_pack FROM sa26i_bin_alloc WHERE doc_no = '"||p_doc_no||"'"
				DECLARE cur_crn CURSOR FOR qty_crn

				LET l_cnt = 1

				FOREACH cur_crn INTO la_stk[l_cnt].*
					LET l_cnt = l_cnt + 1
				END FOREACH

				CALL la_stk.deleteElement(l_cnt)

			--UPDATE for bin qty
				FOR l_cnt = 1 TO la_stk.getLength()
				
					IF ( la_stk[l_cnt].bin_alloc <> 0 ) THEN
					--st02b update
						CALL lf_update_st02b ( p_loc, p_whs, la_stk[l_cnt].stk_code, la_stk[l_cnt].bin_no, "bin_qty_to_pull_pack", la_stk[l_cnt].bin_alloc, "-", p_err_src ) RETURNING g_not_used_string
						CALL lf_update_st02b ( p_loc, p_whs, la_stk[l_cnt].stk_code, la_stk[l_cnt].bin_no, "bin_qty", la_stk[l_cnt].bin_alloc, "+", p_err_src ) RETURNING g_not_used_string
					--sa26i update
						EXECUTE IMMEDIATE "UPDATE sa26i_bin_alloc SET bin_qty_to_pull_pack = 0 WHERE doc_no = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"' AND bin_no = '"||la_stk[l_cnt].bin_no||"'"

					--st30i update
						LET lr_st30i.stk_code		= la_stk[l_cnt].stk_code
                        LET lr_st30i.st30_row_id    = lf_get_field_value ( "st30_stk_tran", "FIRST 1 row_id", "tran_type = 'CRN' AND ref_1 = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"'", p_err_src)
                        CALL lf_increment( "st01_mast", "last_st30i_row_id", "stk_code = '"||la_stk[l_cnt].stk_code||"'", 0, 0 ) RETURNING g_not_used,  lr_st30i.row_id
                        LET lr_st30i.loc			= p_loc
                        LET lr_st30i.whs			= p_whs
                        LET lr_st30i.period			= gr_st00.period
                        LET lr_st30i.tran_type		= "CRN"
                        LET lr_st30i.tran_date		= TODAY
                        LET lr_st30i.tran_time		= CURRENT HOUR TO SECOND
                        LET lr_st30i.create_by		= lf_get_field_value ( "st30_stk_tran", "FIRST 1 create_by", "tran_type = 'CRN' AND ref_1 = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"'", p_err_src)
                        LET lr_st30i.batch_no		= NULL
                        LET lr_st30i.ref_1			= p_doc_no
                        LET lr_st30i.ref_2			= la_stk[l_cnt].bin_no
                        LET lr_st30i.uom			= lf_get_field_value ( "st30_stk_tran", "FIRST 1 uom", "tran_type = 'CRN' AND ref_1 = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"'", p_err_src)
                        LET lr_st30i.qty			= la_stk[l_cnt].bin_alloc
                        LET lr_st30i.phy_bal		= lf_get_field_value("st02b_loc_bins","bin_qty","stk_code = '"||la_stk[l_cnt].stk_code||"' AND loc = '"||p_loc||"' AND whs = '"||p_whs||"' AND bin_no = '"||la_stk[l_cnt].bin_no||"'", p_err_src)

						IF NOT( lf_update_st30i(	{r_st30i}			base.TypeInfo.create(lr_st30i),
													{action_selected}	"create",
													{err_src}			p_err_src ) )
						THEN
							LET p_err_src = lf_sql_error ( p_err_src ||" > UPDATE st30i ", la_stk[l_cnt].stk_code )
							ROLLBACK WORK
							CALL fgl_winmessage( %"Error", p_err_src, "stop" )
							RETURN
						END IF
							
					END IF
				
				END FOR

			WHEN "IBT"
			--BUILD Bin Array of stk codes to update
				PREPARE qty_ibt	FROM "SELECT bin_no, stk_code, bin_qty_to_pull_pack, ib25_row_id FROM ib25i_bin_alloc WHERE ibt_no = '"||p_doc_no||"'"
				DECLARE cur_ibt CURSOR FOR qty_ibt

				LET l_cnt = 1

				FOREACH cur_ibt INTO la_stk[l_cnt].*
					LET l_cnt = l_cnt + 1
				END FOREACH

				CALL la_stk.deleteElement(l_cnt)

			--UPDATE for bin qty
				FOR l_cnt = 1 TO la_stk.getLength()
				
					IF ( la_stk[l_cnt].bin_alloc <> 0 ) THEN
					--st02b update
						CALL lf_update_st02b ( p_loc, p_whs, la_stk[l_cnt].stk_code, la_stk[l_cnt].bin_no, "bin_qty_to_pull_pack", la_stk[l_cnt].bin_alloc, "-", p_err_src ) RETURNING g_not_used_string
						CALL lf_update_st02b ( p_loc, p_whs, la_stk[l_cnt].stk_code, la_stk[l_cnt].bin_no, "bin_qty", la_stk[l_cnt].bin_alloc, "+", p_err_src ) RETURNING g_not_used_string
					--ib25i update
						EXECUTE IMMEDIATE "UPDATE ib25i_bin_alloc SET bin_qty_to_pull_pack = 0 WHERE ibt_no = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"' AND bin_no = '"||la_stk[l_cnt].bin_no||"'"

					--st30i update
						LET lr_st30i.stk_code		= la_stk[l_cnt].stk_code
                        LET lr_st30i.st30_row_id    = 0
                        CALL lf_increment( "st01_mast", "last_st30i_row_id", "stk_code = '"||la_stk[l_cnt].stk_code||"'", 0, 0 ) RETURNING g_not_used,  lr_st30i.row_id
                        LET lr_st30i.loc			= p_loc
                        LET lr_st30i.whs			= p_whs
                        LET lr_st30i.period			= gr_st00.period
                        LET lr_st30i.tran_type		= "IBT"
                        LET lr_st30i.tran_date		= TODAY
                        LET lr_st30i.tran_time		= CURRENT HOUR TO SECOND
                        LET lr_st30i.create_by		= lf_get_field_value ( "ib24_ib_hd", "ibt_user", "ibt_no = '"||p_doc_no||"'", p_err_src )
                        LET lr_st30i.batch_no		= NULL
                        LET lr_st30i.ref_1			= p_doc_no
                        LET lr_st30i.ref_2			= la_stk[l_cnt].bin_no
                        LET lr_st30i.uom			= lf_get_field_value ( "ib25_ib_dt", "uom", "ibt_no = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"' and row_id = '"||la_stk[l_cnt].row_id||"'", p_err_src )
                        LET lr_st30i.qty			= la_stk[l_cnt].bin_alloc
                        LET lr_st30i.phy_bal		= lf_get_field_value("st02b_loc_bins","bin_qty","stk_code = '"||la_stk[l_cnt].stk_code||"' AND loc = '"||p_loc||"' AND whs = '"||p_whs||"' AND bin_no = '"||la_stk[l_cnt].bin_no||"'", p_err_src) 

						IF NOT( lf_update_st30i(	{r_st30i}			base.TypeInfo.create(lr_st30i),
													{action_selected}	"create",
													{err_src}			p_err_src ) )
						THEN
							LET p_err_src = lf_sql_error ( p_err_src ||" > UPDATE st30i ", la_stk[l_cnt].stk_code )
							ROLLBACK WORK
							CALL fgl_winmessage( %"Error", p_err_src, "stop" )
							RETURN
						END IF
							
					END IF
				
				END FOR

			WHEN "IBT"
			--BUILD Bin Array of stk codes to update
				CALL lf_build_array_from_string( p_doc_no, la_string, ",") RETURNING g_not_used_string

				LET l_ship_doc 	= la_string[1]
				LET l_ship_no 	= la_string[2]
			
				PREPARE qty_ibp	FROM "SELECT bin_no, stk_code, bin_qty_to_pull_pack, ship_doc_no FROM ib31i_bin_alloc WHERE ibt_no = '"||l_ship_doc||"'"
				DECLARE cur_ibp CURSOR FOR qty_ibp

				LET l_cnt = 1

				FOREACH cur_ibp INTO la_stk_ibp[l_cnt].*
					LET l_cnt = l_cnt + 1
					
				END FOREACH

				CALL la_stk_ibp.deleteElement(l_cnt)

			--UPDATE for bin qty
				FOR l_cnt = 1 TO la_stk_ibp.getLength()
				
					IF ( la_stk_ibp[l_cnt].bin_alloc <> 0 AND la_stk_ibp[l_cnt].ship_no = l_ship_no ) THEN
					--st02b update
						CALL lf_update_st02b ( p_loc, p_whs, la_stk_ibp[l_cnt].stk_code, la_stk_ibp[l_cnt].bin_no, "bin_qty_to_pull_pack", la_stk_ibp[l_cnt].bin_alloc, "-", p_err_src ) RETURNING g_not_used_string
						CALL lf_update_st02b ( p_loc, p_whs, la_stk_ibp[l_cnt].stk_code, la_stk_ibp[l_cnt].bin_no, "bin_qty", la_stk_ibp[l_cnt].bin_alloc, "+", p_err_src ) RETURNING g_not_used_string
					--ib31i update
						EXECUTE IMMEDIATE "UPDATE ib31i_bin_alloc SET bin_qty_to_pull_pack = 0 WHERE ibt_no = '"||l_ship_doc||"' AND stk_code = '"||la_stk_ibp[l_cnt].stk_code||"' AND bin_no = '"||la_stk_ibp[l_cnt].bin_no||"' AND ship_doc_no = '"||l_ship_no||"'"

					--st30i update
						LET lr_st30i.stk_code		= la_stk_ibp[l_cnt].stk_code
                        LET lr_st30i.st30_row_id    = lf_get_field_value ( "st30_stk_tran", "FIRST 1 row_id", "tran_type = 'IBT' AND ref_1 = '"||p_doc_no||"' AND stk_code = '"||la_stk_ibp[l_cnt].stk_code||"'", p_err_src)
                        CALL lf_increment( "st01_mast", "last_st30i_row_id", "stk_code = '"||la_stk_ibp[l_cnt].stk_code||"'", 0, 0 ) RETURNING g_not_used,  lr_st30i.row_id
                        LET lr_st30i.loc			= p_loc
                        LET lr_st30i.whs			= p_whs
                        LET lr_st30i.period			= gr_st00.period
                        LET lr_st30i.tran_type		= "IBT"
                        LET lr_st30i.tran_date		= TODAY
                        LET lr_st30i.tran_time		= CURRENT HOUR TO SECOND
                        LET lr_st30i.create_by		= lf_get_field_value ( "st30_stk_tran", "FIRST 1 create_by", "tran_type = 'IBT' AND ref_1 = '"||l_ship_doc||"' AND stk_code = '"||la_stk_ibp[l_cnt].stk_code||"'", p_err_src)
                        LET lr_st30i.batch_no		= NULL
                        LET lr_st30i.ref_1			= p_doc_no
                        LET lr_st30i.ref_2			= la_stk_ibp[l_cnt].bin_no
                        LET lr_st30i.uom			= lf_get_field_value ( "st30_stk_tran", "FIRST 1 uom", "tran_type = 'IBT' AND ref_1 = '"||p_doc_no||"' AND stk_code = '"||la_stk_ibp[l_cnt].stk_code||"'", p_err_src)
                        LET lr_st30i.qty			= la_stk_ibp[l_cnt].bin_alloc
                        LET lr_st30i.phy_bal		= lf_get_field_value("st02b_loc_bins","bin_qty","stk_code = '"||la_stk_ibp[l_cnt].stk_code||"' AND loc = '"||p_loc||"' AND whs = '"||p_whs||"' AND bin_no = '"||la_stk_ibp[l_cnt].bin_no||"'", p_err_src)

						IF NOT( lf_update_st30i(	{r_st30i}			base.TypeInfo.create(lr_st30i),
													{action_selected}	"create",
													{err_src}			p_err_src ) )
						THEN
							LET p_err_src = lf_sql_error ( p_err_src ||" > UPDATE st30i ", la_stk_ibp[l_cnt].stk_code )
							ROLLBACK WORK
							CALL fgl_winmessage( %"Error", p_err_src, "stop" )
							RETURN
						END IF
							
					END IF
				
				END FOR

			WHEN "GRV"
			--BUILD Bin Array of stk codes to update
				PREPARE qty_grv	FROM "SELECT bin_no, stk_code, bin_qty_to_pull_pack FROM pu26i_bin_alloc WHERE doc_no = '"||p_doc_no||"'"
				DECLARE cur_grv CURSOR FOR qty_grv

				LET l_cnt = 1

				FOREACH cur_grv INTO la_stk[l_cnt].*
					LET l_cnt = l_cnt + 1
				END FOREACH

				CALL la_stk.deleteElement(l_cnt)

			--UPDATE for bin qty
				FOR l_cnt = 1 TO la_stk.getLength()
				
					IF ( la_stk[l_cnt].bin_alloc <> 0 ) THEN
					--st02b update
						CALL lf_update_st02b ( p_loc, p_whs, la_stk[l_cnt].stk_code, la_stk[l_cnt].bin_no, "bin_qty_to_pull_pack", la_stk[l_cnt].bin_alloc, "-", p_err_src ) RETURNING g_not_used_string
						CALL lf_update_st02b ( p_loc, p_whs, la_stk[l_cnt].stk_code, la_stk[l_cnt].bin_no, "bin_qty", la_stk[l_cnt].bin_alloc, "+", p_err_src ) RETURNING g_not_used_string

					--pu26i update
						EXECUTE IMMEDIATE "UPDATE pu26i_bin_alloc SET bin_qty_to_pull_pack = 0 WHERE doc_no = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"' AND bin_no = '"||la_stk[l_cnt].bin_no||"'"

					--st30i update
						LET lr_st30i.stk_code		= la_stk[l_cnt].stk_code
                        LET lr_st30i.st30_row_id    = lf_get_field_value ( "st30_stk_tran", "FIRST 1 row_id", "tran_type = 'GRV' AND ref_1 = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"'", p_err_src)
                        CALL lf_increment( "st01_mast", "last_st30i_row_id", "stk_code = '"||la_stk[l_cnt].stk_code||"'", 0, 0 ) RETURNING g_not_used,  lr_st30i.row_id
                        LET lr_st30i.loc			= p_loc
                        LET lr_st30i.whs			= p_whs
                        LET lr_st30i.period			= gr_st00.period
                        LET lr_st30i.tran_type		= "GRV"
                        LET lr_st30i.tran_date		= TODAY
                        LET lr_st30i.tran_time		= CURRENT HOUR TO SECOND
                        LET lr_st30i.create_by		= lf_get_field_value ( "st30_stk_tran", "FIRST 1 create_by", "tran_type = 'GRV' AND ref_1 = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"'", p_err_src)
                        LET lr_st30i.batch_no		= NULL
                        LET lr_st30i.ref_1			= p_doc_no
                        LET lr_st30i.ref_2			= la_stk[l_cnt].bin_no
                        LET lr_st30i.uom			= lf_get_field_value ( "st30_stk_tran", "FIRST 1 uom", "tran_type = 'GRV' AND ref_1 = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"'", p_err_src)
                        LET lr_st30i.qty			= la_stk[l_cnt].bin_alloc
                        LET lr_st30i.phy_bal		= lf_get_field_value("st02b_loc_bins","bin_qty","stk_code = '"||la_stk[l_cnt].stk_code||"' AND loc = '"||p_loc||"' AND whs = '"||p_whs||"' AND bin_no = '"||la_stk[l_cnt].bin_no||"'", p_err_src)

						IF NOT( lf_update_st30i(	{r_st30i}			base.TypeInfo.create(lr_st30i),
													{action_selected}	"create",
													{err_src}			p_err_src ) )
						THEN
							LET p_err_src = lf_sql_error ( p_err_src ||" > UPDATE st30i ", la_stk[l_cnt].stk_code )
							ROLLBACK WORK
							CALL fgl_winmessage( %"Error", p_err_src, "stop" )
							RETURN
						END IF
							
					END IF
				
				END FOR			

			WHEN "GRN"
			--BUILD Bin Array of stk codes to update
				PREPARE qty_grn	FROM "SELECT bin_no, stk_code, bin_qty_to_pull_pack FROM pu26i_bin_alloc WHERE doc_no = '"||p_doc_no||"'"
				DECLARE cur_grn CURSOR FOR qty_grn

				LET l_cnt = 1

				FOREACH cur_grn INTO la_stk[l_cnt].*
					LET l_cnt = l_cnt + 1
				END FOREACH

				CALL la_stk.deleteElement(l_cnt)

			--UPDATE for bin qty
				FOR l_cnt = 1 TO la_stk.getLength()
				
					IF ( la_stk[l_cnt].bin_alloc <> 0 ) THEN
					--st02b update
						CALL lf_update_st02b ( p_loc, p_whs, la_stk[l_cnt].stk_code, la_stk[l_cnt].bin_no, "bin_qty_to_pull_pack", la_stk[l_cnt].bin_alloc, "-", p_err_src ) RETURNING g_not_used_string
						CALL lf_update_st02b ( p_loc, p_whs, la_stk[l_cnt].stk_code, la_stk[l_cnt].bin_no, "bin_qty", la_stk[l_cnt].bin_alloc, "+", p_err_src ) RETURNING g_not_used_string

					--pu26i update
						EXECUTE IMMEDIATE "UPDATE pu26i_bin_alloc SET bin_qty_to_pull_pack = 0 WHERE doc_no = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"' AND bin_no = '"||la_stk[l_cnt].bin_no||"'"

					--st30i update
						LET lr_st30i.stk_code		= la_stk[l_cnt].stk_code
                        LET lr_st30i.st30_row_id    = lf_get_field_value ( "st30_stk_tran", "FIRST 1 row_id", "tran_type = 'GRN' AND ref_1 = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"'", p_err_src)
                        CALL lf_increment( "st01_mast", "last_st30i_row_id", "stk_code = '"||la_stk[l_cnt].stk_code||"'", 0, 0 ) RETURNING g_not_used,  lr_st30i.row_id
                        LET lr_st30i.loc			= p_loc
                        LET lr_st30i.whs			= p_whs
                        LET lr_st30i.period			= gr_st00.period
                        LET lr_st30i.tran_type		= "GRN"
                        LET lr_st30i.tran_date		= TODAY
                        LET lr_st30i.tran_time		= CURRENT HOUR TO SECOND
                        LET lr_st30i.create_by		= lf_get_field_value ( "st30_stk_tran", "FIRST 1 create_by", "tran_type = 'GRN' AND ref_1 = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"'", p_err_src)
                        LET lr_st30i.batch_no		= NULL
                        LET lr_st30i.ref_1			= p_doc_no
                        LET lr_st30i.ref_2			= la_stk[l_cnt].bin_no
                        LET lr_st30i.uom			= lf_get_field_value ( "st30_stk_tran", "FIRST 1 uom", "tran_type = 'GRN' AND ref_1 = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"'", p_err_src)
                        LET lr_st30i.qty			= la_stk[l_cnt].bin_alloc
                        LET lr_st30i.phy_bal		= lf_get_field_value("st02b_loc_bins","bin_qty","stk_code = '"||la_stk[l_cnt].stk_code||"' AND loc = '"||p_loc||"' AND whs = '"||p_whs||"' AND bin_no = '"||la_stk[l_cnt].bin_no||"'", p_err_src)

						IF NOT( lf_update_st30i(	{r_st30i}			base.TypeInfo.create(lr_st30i),
													{action_selected}	"create",
													{err_src}			p_err_src ) )
						THEN
							LET p_err_src = lf_sql_error ( p_err_src ||" > UPDATE st30i ", la_stk[l_cnt].stk_code )
							ROLLBACK WORK
							CALL fgl_winmessage( %"Error", p_err_src, "stop" )
							RETURN
						END IF
							
					END IF
				
				END FOR

			WHEN "BOM"
			--BUILD Bin Array of stk codes to update
				PREPARE qty_bom	FROM "SELECT bin_no, stk_code, bin_qty_to_pull_pack FROM bm20i_bin_alloc WHERE wo_no = '"||p_doc_no||"'"
				DECLARE cur_bom CURSOR FOR qty_bom

				LET l_cnt = 1

				FOREACH cur_bom INTO la_stk[l_cnt].*
					LET l_cnt = l_cnt + 1
				END FOREACH

				CALL la_stk.deleteElement(l_cnt)

			--UPDATE for bin qty
				FOR l_cnt = 1 TO la_stk.getLength()
				
					IF ( la_stk[l_cnt].bin_alloc <> 0 ) THEN
					--st02b update
						CALL lf_update_st02b ( p_loc, p_whs, la_stk[l_cnt].stk_code, la_stk[l_cnt].bin_no, "bin_qty_to_pull_pack", la_stk[l_cnt].bin_alloc, "-", p_err_src ) RETURNING g_not_used_string
						CALL lf_update_st02b ( p_loc, p_whs, la_stk[l_cnt].stk_code, la_stk[l_cnt].bin_no, "bin_qty", la_stk[l_cnt].bin_alloc, "+", p_err_src ) RETURNING g_not_used_string

					--bm20i update
						EXECUTE IMMEDIATE "UPDATE bm20i_bin_alloc SET bin_qty_to_pull_pack = 0 WHERE wo_no = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"' AND bin_no = '"||la_stk[l_cnt].bin_no||"'"

					--st30i update
						LET lr_st30i.stk_code		= la_stk[l_cnt].stk_code
                        LET lr_st30i.st30_row_id    = lf_get_field_value ( "st30_stk_tran", "FIRST 1 row_id", "tran_type = 'W/O' AND ref_1 = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"'", p_err_src)
                        CALL lf_increment( "st01_mast", "last_st30i_row_id", "stk_code = '"||la_stk[l_cnt].stk_code||"'", 0, 0 ) RETURNING g_not_used,  lr_st30i.row_id
                        LET lr_st30i.loc			= p_loc
                        LET lr_st30i.whs			= p_whs
                        LET lr_st30i.period			= gr_st00.period
                        LET lr_st30i.tran_type		= "W/O"
                        LET lr_st30i.tran_date		= TODAY
                        LET lr_st30i.tran_time		= CURRENT HOUR TO SECOND
                        LET lr_st30i.create_by		= lf_get_field_value ( "st30_stk_tran", "FIRST 1 create_by", "tran_type = 'W/O' AND ref_1 = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"'", p_err_src)
                        LET lr_st30i.batch_no		= lf_get_field_value ( "st30_stk_tran", "FIRST 1 batch_no", "tran_type = 'W/O' AND ref_1 = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"'", p_err_src)
                        LET lr_st30i.ref_1			= p_doc_no
                        LET lr_st30i.ref_2			= la_stk[l_cnt].bin_no
                        LET lr_st30i.uom			= lf_get_field_value ( "st30_stk_tran", "FIRST 1 uom", "tran_type = 'W/O' AND ref_1 = '"||p_doc_no||"' AND stk_code = '"||la_stk[l_cnt].stk_code||"'", p_err_src)
                        LET lr_st30i.qty			= la_stk[l_cnt].bin_alloc
                        LET lr_st30i.phy_bal		= lf_get_field_value("st02b_loc_bins","bin_qty","stk_code = '"||la_stk[l_cnt].stk_code||"' AND loc = '"||p_loc||"' AND whs = '"||p_whs||"' AND bin_no = '"||la_stk[l_cnt].bin_no||"'", p_err_src)

						IF NOT( lf_update_st30i(	{r_st30i}			base.TypeInfo.create(lr_st30i),
													{action_selected}	"create",
													{err_src}			p_err_src ) )
						THEN
							LET p_err_src = lf_sql_error ( p_err_src ||" > UPDATE st30i ", la_stk[l_cnt].stk_code )
							ROLLBACK WORK
							CALL fgl_winmessage( %"Error", p_err_src, "stop" )
							RETURN
						END IF
							
					END IF
				
				END FOR			
			
		END CASE

		COMMIT WORK

		RETURN
			
	CATCH
		LET p_err_src = lf_sql_error ( p_err_src ||" > UPDATE upd_bin_qty ", la_stk[l_cnt].stk_code )
		ROLLBACK WORK
		CALL fgl_winmessage( %"Error", p_err_src, "stop" )
		RETURN
	END TRY	

END FUNCTION


{==================================================================================================================================}
{==================================================================================================================================}
{
													END UPDATE FUNCTIONS
}
{==================================================================================================================================}
{==================================================================================================================================}


{==================================================================================================================================}
{==================================================================================================================================}
--##################################################################################################################################
{
													GENERAL FUNCTIONS
} 
--##################################################################################################################################
{==================================================================================================================================}
{==================================================================================================================================}
FUNCTION GENERAL_FUNCTIONS()
END FUNCTION
{==================================================================================================================================}
#+ TEST IF TASK EXISTS
#+
#+ BUSINESS RULE:
#+ Tests if the current task is open
#+
#+
#+ @code CALL sb_task_exists( p_err_src )
#+
#+ @param p_err_src Error tracking variable
#+
#+ @return NONE
#+
#+ CHANGES
#+
FUNCTION sb_task_exists( p_err_src )

    DEFINE	p_err_src	STRING,

            l_doc_no    VARCHAR(14)

	LET p_err_src = p_err_src , " > sb_task_exists"

    IF ( mr_st40_action.doc_type = "IBT" ) THEN 
        LET l_doc_no = mr_st40_action.doc_no||","||mr_st40_action.ship_doc_no
    ELSE 
        LET l_doc_no = mr_st40_action.doc_no
    END IF 
    
--Determine if new or existing task
	IF ( lf_row_exists(	{tbl_name}		"st40_track_store_pulling",
						{where_clause}	"doc_type"	||" = "	|| "'"|| mr_st40_action.doc_type	||"'"	||" AND "||
										"doc_no"	||" = "	|| "'"|| l_doc_no	                ||"'"	||" AND "||
										"end_date"	||" IS "||		 "NULL"								||" AND "||
										"action_by"	||" = "	|| "'"|| mr_st40_action.user_name	||"'",
						{err_src}		p_err_src ) )
	THEN
	--Fetch data for existing task
		RETURN TRUE
	ELSE
	--Create New Task
		RETURN FALSE
	END IF

END FUNCTION

{==================================================================================================================================}
#+ TEST IF TASK EXISTS
#+
#+ BUSINESS RULE:
#+ Tests if the current task is open
#+
#+
#+ @code CALL sb_force_assign_user_true( p_next_phase, p_whs_phase_sort, p_loc, p_whs, p_doc_type, p_doc_no , p_err_src )
#+
#+ @param p_err_src Error tracking variable
#+
#+ @return NONE
#+
#+ CHANGES
#+

FUNCTION sb_force_assign_user_true( p_next_phase, p_whs_phase_sort, p_loc, p_whs, p_doc_type, p_doc_no , p_err_src )

    DEFINE  p_next_phase        BOOLEAN,
            p_whs_phase_sort    LIKE st40_track_store_pulling.whs_phase_sort,
            p_loc               LIKE st40_track_store_pulling.loc,
            p_whs               LIKE st40_track_store_pulling.whs,
            p_doc_type          LIKE st40_track_store_pulling.doc_type,
            p_doc_no            LIKE st40_track_store_pulling.doc_no,
            p_err_src           STRING,

            l_priority_level    LIKE st40_track_store_pulling.priority_lvl

    -- 1) Check to see if the next phase or current phase is force_assign_user = 'Y' 
    -- 2) Check if this priority has been actioned by a NON-Phase user
    {
        -- If both of these checks are true then we skip the current 
    }
    IF ( p_next_phase = TRUE ) THEN 
        LET p_whs_phase_sort = p_whs_phase_sort +1
    END IF 
    
    --Fetch original row priority lvl
    PREPARE qry_st40_org_pri_lvl FROM	"SELECT "							||
                                            "priority_lvl "			        ||
                                        "FROM "							    ||
                                            "st40_track_store_pulling "	    ||
                                        "WHERE "							||
                                                "doc_type = '"|| p_doc_type	||"'"	||" AND "||
                                                "doc_no = '"|| p_doc_no	    ||"'"	||" AND "||
                                                "row_id = (SELECT MIN(row_id) FROM st40_track_store_pulling WHERE doc_type = '"|| p_doc_type ||"' AND doc_no = '"|| p_doc_no||"')"

    EXECUTE qry_st40_org_pri_lvl INTO l_priority_level

    -- 1)
    IF ( lf_row_exists(	"st40u_warehouse_user", "whs_phase_user = 'Y' AND whs_phase_sort = " || p_whs_phase_sort ||" AND force_assign_user = 'Y' AND priority_level like '%"||l_priority_level||"%'", p_err_src ) )THEN 

        -- 2)
        IF ( lf_row_exists(	{tbl_name}		"st40_track_store_pulling",
                            {where_clause}	"loc = '"	        || p_loc		        ||"' AND "||
                                            "whs = '"	        || p_whs		        ||"' AND "||
                                            "doc_type = '"	    || p_doc_type	        ||"' AND "||
                                            "doc_no = '"	    || p_doc_no	            ||"' AND "||
                                            "action_by IN ( SELECT user_name FROM st40u_warehouse_user WHERE whs_phase_user = 'N')"||" AND "||
                                            "end_date IS NULL "                                                     ||" AND " ||
                                            "end_time IS NULL ",
                            {err_src}		p_err_src ) )
        THEN
            RETURN TRUE 
        END IF 
    END IF 

    RETURN FALSE 
    
END FUNCTION 

 {==================================================================================================================================}
#+ OPEN SELECTED PROGRAM 
#+
#+ BUSINESS RULE:
#+ Open up the selected program ( button that the user clicked ) 
#+
#+ @code CALL sb_open_receiving_program( p_cr, p_action_selected, p_err_src )
#+
#+ @param p_err_src Error tracking variable
#+
#+ @return NONE
#+
FUNCTION sb_open_receiving_program( p_cr, p_action_selected, p_err_src )

    DEFINE  p_cr                    INTEGER,
            p_action_selected       STRING,
            p_err_src               STRING

    LET p_err_src = p_err_src ||" > sb_open_receiving_program "

    -- If the array current is Greater than 0 then set that record to the default color
    IF ( p_cr > 0 ) THEN
        CALL bld_st40_tasks_attribute( lf_get_field_value( "st40u_warehouse_user", "whs_color", "whs_status = '"|| ma_st40_priority[ARR_CURR()].status ||"' AND loc IN ('00','"||gr_sy02.default_loc||"') AND whs IN ('00','"||gr_sy02.default_whs||"') AND type = 'R'", p_err_src ) || " reverse", ma_st40_priority_att[ARR_CURR()].* ) RETURNING ma_st40_priority_att[ARR_CURR()].*
    END IF
    
    CASE p_action_selected

        WHEN "receive_ibt"  -- Receiving an IBT
            RUN "fglrun ib140_ship.42r " || g_schema_name || " " || g_user_name || " Z Z IBTR"

        WHEN "direct_c_notes" -- direct Credit Notes
            RUN "fglrun sa135_inv.42r " || g_schema_name || " " || g_user_name || " Z Z DCRN Z Z Z" 

        WHEN "linked_c_notes" -- Linked Credit Notes
            RUN "fglrun sa135_inv.42r " || g_schema_name || " " || g_user_name || " Z Z LCRN Z Z Z"

        WHEN "direct_grn" -- Direct GRN
            RUN "fglrun pu135_grn.42r " || g_schema_name || " " || g_user_name || " Z Z DGRN " 

        WHEN "grn_a_po" -- GRN a PO 
            IF ( p_cr = 0 ) THEN 
                RUN "fglrun pu135_grn.42r " || g_schema_name || " " || g_user_name || " Z Z GRNP " 
            ELSE 
                IF ( ma_st40_priority[p_cr].doc_type = "PO" ) THEN 
                    RUN "fglrun pu135_grn.42r " || g_schema_name || " " || g_user_name || " Z "||ma_st40_priority[p_cr].doc_no||" GRNP " 
                ELSE 
                    RUN "fglrun pu135_grn.42r " || g_schema_name || " " || g_user_name || " Z Z GRNP " 
                END IF 
            END IF  

        WHEN "po_rec" -- Purchase Order Receiving
            RUN "fglrun st153_req_col.42r " || g_schema_name || " " || g_user_name || " Z Z R Z"

    END CASE 
                    
    -- Build/Refresh Receiving Table
    CALL bld_and_display_data( NULL,"default_order", p_err_src )
    CALL ui.Interface.refresh()  

    -- Highlight the first Row ( If there is records ) 
    -- Also do NOT highlight when in scanner mode ( scanner mode does its own highlighting logic when switching to ENQ mode ) 
    IF ( ma_st40_priority.getLength() > 0 ) AND ( m_scanner_mode = FALSE )THEN
        CALL fgl_set_arr_curr( 1 )
        CALL sb_set_cyan_highlights( 1, p_err_src )
    END IF 

END FUNCTION 

{==================================================================================================================================}
#+ RECOVER INCOMPLETE INPUT
#+
#+ BUSINESS RULE:
#+ Amend incomplete records
#+
#+
#+ @code    CALL sb_recover_incomplete_input( err_src )
#+
#+ @param   err_src
#+
#+ @return  NONE
#+
#+ CHANGES
#+
FUNCTION sb_recover_incomplete_input( p_err_src )

	DEFINE  p_err_src 			STRING
			
	LET p_err_src = p_err_src || " > sb_recover_incomplete_input"

    TRY 
        IF ( m_incomplete = "recovery" ) THEN 
            CALL lf_update_in_use_log( "U", "st117_bin_loc_maint", "B,"||gr_sy02.default_loc||","||gr_sy02.default_loc, p_err_src ) RETURNING g_not_used
        END IF 
    CATCH 
		LET p_err_src = lf_sql_error ( p_err_src, " > sb_bld_module_init") 
		CALL fgl_winmessage( %"Error", p_err_src, "stop" ) 
	END TRY
    
END FUNCTION 

{==================================================================================================================================}
{
													END GENERAL FUNCTIONS
}
{==================================================================================================================================}
{==================================================================================================================================}


{==================================================================================================================================}
{==================================================================================================================================}
--##################################################################################################################################
{
													INITIALISATION FUNCTIONS
} 
--##################################################################################################################################
{==================================================================================================================================}
{==================================================================================================================================}
FUNCTION INITIALISATION_FUNCTIONS()
END FUNCTION
{==================================================================================================================================}
#+ MODULAR INIT
#+
#+ BUSINESS RULE:
#+ Initialises any program specific variables.
#+
#+
#+ @code CALL sb_module_init( p_err_src )
#+
#+ @param p_err_src Error tracking variable
#+
#+ @return NONE
#+
#+ CHANGES
#+
FUNCTION sb_module_init( p_err_src )

    DEFINE	p_err_src	STRING

	LET p_err_src = p_err_src , " > sb_module_init"

	LET w_cur = ui.Window.getCurrent()
	LET f_cur = w_cur.getForm()

	CASE ( m_prog_type )

		WHEN "LOG"--Log an Action
		--Set titles
			CALL w_cur.setText( "Receiving Logging" )
			LET m_form_title = "RECEIVING LOGGING"
			
		OTHERWISE
			CALL fgl_winmessage( %"Invalid Program", %"You have selected an invalid program!\nPlease contact support.", "exclamation" )
			EXIT PROGRAM
	
	END CASE

END FUNCTION
{==================================================================================================================================}
#+ BUILD COMBOBOX FOR DOC ACTIONS
#+
#+ BUSINESS RULE:
#+ Builds the combobox for the status field with db value.
#+
#+ @code CALL sb_cbox_build_action(  p_doc_no, p_action_by, p_cur_status )
#+
#+ @param   p_doc_no        Current Doc No
#+ @param   p_action_by     Action by what user
#+ @param   p_cur_status    Current Status of the document
#+
#+ @return NONE
#+
#+ CHANGES
#+
#+ 20. 18/11/2019 Cato P        [49316] 8.) WMS - Dispatching Dashboard
#+                              New warehouse Dispatch improvements
#+
FUNCTION sb_cbox_build_action( p_doc_no, p_action_by, p_cur_status )
			
    DEFINE  p_doc_no                LIKE st40_track_store_pulling.doc_no,
            p_cur_status            LIKE st40_track_store_pulling.status,
            p_action_by             LIKE st40_track_store_pulling.action_by
            
	DEFINE  l_cur_phase             STRING,
            l_next_phase            STRING,
            l_err_src               STRING 

    LET l_err_src = " sb_cbox_build_action"
    
    LET l_cur_phase = lf_get_field_value( "st40u_warehouse_user", "full_name","whs_phase_user = 'Y' AND type = 'R' AND whs_phase_sort = '"||p_cur_status||"'", l_err_src )

    LET l_next_phase = lf_get_field_value( "st40u_warehouse_user", "FIRST 1 full_name","whs_phase_user = 'Y' AND type = 'R' AND whs_phase_sort > '"||p_cur_status||"' AND po_hold = 'N' AND cd_hold = 'N' ORDER BY whs_phase_sort", l_err_src )

    -- Display Current Phase
    DISPLAY l_cur_phase TO cur_phase

    IF ( lf_row_exists( "st40u_warehouse_user","whs_phase_user = 'Y' AND type = 'R' AND whs_phase_sort = '"||p_cur_status||"' AND force_assign_user = 'Y'",l_err_src) ) THEN
        IF ( lf_row_exists ( "st40u_warehouse_user"," user_name = '"||p_action_by||"' AND whs_phase_user = 'N' ",l_err_src )) THEN 
            DISPLAY l_next_phase || " ( If users are completed )" TO next_phase

        ELSE 
            DISPLAY l_cur_phase ||" ( Assign a user )" TO next_phase
        END IF 
    ELSE 
        -- Display Next Phase
        IF ( lf_row_exists( "st40u_warehouse_user","whs_phase_user = 'Y' AND type = 'R' AND full_name = '"||l_next_phase||"' AND force_assign_user = 'Y'",l_err_src) ) THEN
            DISPLAY l_next_phase ||" ( Assign a user )" TO next_phase
        ELSE 
            DISPLAY l_next_phase TO next_phase
        END IF 
    END IF  

END FUNCTION

{==================================================================================================================================}
#+ PROGRAM CBOX_BUILD
#+
#+ BUSINESS RULE:
#+ Build's all module specific comboboxes
#+
#+
#+ @code CALL sb_bld_cbox_build( p_cbox )
#+
#+ @param   p_cbox
#+
#+ @return  NONE
#+
#+ CHANGES
#+

FUNCTION sb_bld_cbox_build( p_cbox )

    DEFINE  p_cbox			        ui.combobox,
            l_cbox_tag		        CHAR(30),
			l_cnt			        INTEGER				--Counter for array insert

	DEFINE  la_whs_users	        DYNAMIC ARRAY OF RECORD --Used to store all the whs phase users and their full descriptions
                                        whs_phase_sort		LIKE st40u_warehouse_user.whs_phase_sort,
                                        full_name			LIKE st40u_warehouse_user.full_name
                                    END RECORD
             
    LET l_cbox_tag 	= p_cbox.gettag()
	LET l_cnt 		= 1		

--Pull all whs users from st40u	
	CALL la_whs_users.clear()

	PREPARE qry_whs_phase FROM "SELECT whs_phase_sort, full_name FROM st40u_warehouse_user WHERE whs_phase_user = 'Y' AND type = 'R'"
	DECLARE curs_whs_phase CURSOR FOR qry_whs_phase

	FOREACH curs_whs_phase INTO la_whs_users[l_cnt].*
		LET l_cnt = l_cnt + 1
	END FOREACH

	CALL la_whs_users.deleteElement(l_cnt)
    
    CASE ( l_cbox_tag )

		WHEN "cbox_doc_type"
		--Descriptions are set in the sb_set_win_title_and_hide_show_fields() function
			CALL p_cbox.clear()
            CALL p_cbox.additem("CRN","CRN - Credit Notes")
			CALL p_cbox.additem("IBT","IBT - Inter Branch Transfer")
            CALL p_cbox.additem("PO","P/O - Purchase Order Received")
			CALL p_cbox.additem("GRN","GRN - Goods Recieved Notes")
			CALL p_cbox.additem("BOM","BOM - Bill Of Materials")
			
		WHEN "cbox_status_display"
		--Descriptions are set in the sb_set_win_title_and_hide_show_fields() function
			CALL p_cbox.clear()
			FOR l_cnt = 1 TO la_whs_users.getLength()
				CALL p_cbox.additem( la_whs_users[l_cnt].whs_phase_sort, la_whs_users[l_cnt].full_name )
			END FOR
			
		WHEN "cbox_status_input"
		--Descriptions are set in the sb_set_win_title_and_hide_show_fields() function
            CALL p_cbox.clear()
			FOR l_cnt = 1 TO la_whs_users.getLength()
				CALL p_cbox.additem( la_whs_users[l_cnt].whs_phase_sort, la_whs_users[l_cnt].full_name )
			END FOR
        
    END CASE

END FUNCTION

{==================================================================================================================================}
#+ BUILD COMBOBOX FOR DOC ENQ STATUS
#+
#+ BUSINESS RULE:
#+ Builds the combobox for the status field with db value.
#+
#+ @code CALL sb_cbox_build_doc_enq()
#+
#+ @param None
#+
#+ @return NONE
#+
#+ CHANGES
#+
FUNCTION sb_cbox_build_doc_enq()
			
	DEFINE  l_cbox					ui.ComboBox,
			l_cnt			        INTEGER,				--Counter for array insert
            l_filter                STRING
            
    DEFINE  la_status		        DYNAMIC ARRAY OF RECORD --Used to store all the whs phase users and their full descriptions
                                        whs_status		    LIKE st40u_warehouse_user.whs_status,
                                        full_name			LIKE st40u_warehouse_user.full_name
                                    END RECORD

    --Pull all whs Status's order by whs phase sort order
	CALL la_status.clear()

    -- Add filters based off what the current document status is
    IF ( lf_row_exists( "st40u_warehouse_user","type = 'R' AND	whs_phase_user = 'Y' AND whs_status = '"||mr_doc_st40.status||"' AND po_hold = 'Y'", "sb_cbox_build_doc_enq" ) ) THEN 
        LET l_filter = "AND (po_hold = 'Y' OR final_phase = 'Y')"
    END IF 

    IF ( lf_row_exists( "st40u_warehouse_user","type = 'R' AND	whs_phase_user = 'Y' AND whs_status = '"||mr_doc_st40.status||"' and cd_hold = 'Y'", "sb_cbox_build_doc_enq" ) ) THEN 
        LET l_filter = "AND (cd_hold = 'Y' OR final_phase = 'Y')"
    END IF 

    IF ( l_filter IS NULL ) THEN 
        LET l_filter = "AND po_hold = 'N' AND cd_hold = 'N'" 
    END IF 
    
	PREPARE qry_whs_status FROM "SELECT whs_status, full_name FROM st40u_warehouse_user WHERE whs_phase_user = 'Y' AND type = 'R' "||l_filter||" ORDER BY whs_phase_sort"
	DECLARE curs_whs_status CURSOR FOR qry_whs_status

    LET l_cnt = 1
	FOREACH curs_whs_status INTO la_status[l_cnt].*
		LET l_cnt = l_cnt + 1
	END FOREACH

	CALL la_status.deleteElement(l_cnt)
				
	LET l_cbox = ui.ComboBox.forName("status")
    
    CALL l_cbox.clear()
    FOR l_cnt = 1 TO la_status.getLength()
        CALL l_cbox.additem( la_status[l_cnt].whs_status, la_status[l_cnt].full_name )
    END FOR
       
END FUNCTION

{==================================================================================================================================}
#+ CONTINUE AFTER IDLE TIMEOUT	#+
#+
#+ BUSINESS RULE: #+
#+ Runs necessary test when amend of record times out	#+
#+
#+ @code    CALL sb_hide_buttons( p_err_src )
#+
#+ @param	p_err_src           Error Source    								#+
#+
#+ @return  TRUE				if user CAN continue with amend of record		#+
#+ @return	FALSE				if user CANNOT continue with amend of record	#+
#+
#+ CHANGES
#+
FUNCTION sb_set_cyan_highlights( p_cr, p_err_src )

	DEFINE	p_cr                INTEGER,
            p_err_src		    STRING,
    
            l_dlog				ui.Dialog

	LET p_err_src = p_err_src || " > sb_hide_buttons"
	LET l_dlog = ui.Dialog.getCurrent()

    IF ( p_cr = 0 ) THEN 
        RETURN 
    END IF 
    
    LET ma_st40_priority_att[p_cr].doc_no = "cyan reverse"
    CALL l_dlog.setArrayAttributes( "priority", ma_st40_priority_att )
    
END FUNCTION



{==================================================================================================================================}
{==================================================================================================================================}
{
												END INITIALISATION FUNCTIONS
}
{==================================================================================================================================}
{==================================================================================================================================}