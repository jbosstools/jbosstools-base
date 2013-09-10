/*************************************************************************************
 * Copyright (c) 2010-2013 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.ui.internal;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.runtime.ui.internal.messages"; //$NON-NLS-1$
	public static String AutoResizeTableLayout_Unknown_column_layout_data;
	public static String DownloadRuntimeDialog_Browse;
	public static String DownloadRuntimeDialog_Delete_archive_after_installing;
	public static String DownloadRuntimeDialog_Download_folder;
	public static String DownloadRuntimeDialog_Install_folder;
	public static String DownloadRuntimeDialog_No_runtime_server_found;
	public static String DownloadRuntimeDialog_Select_a_runtime_to_download;
	public static String DownloadRuntimeDialog_Select_download_folder;
	public static String DownloadRuntimeDialog_Select_install_folder;
	public static String DownloadRuntimeDialog_This_folder_does_not_exist_or_is_not_writable;
	public static String DownloadRuntimeDialog_This_folder_is_required;
	public static String DownloadRuntimeDialog_URL;
	public static String DownloadRuntimeDialog_Warning;
	public static String DownloadRuntimeLicensePage_I_accept_the_terms;
	public static String DownloadRuntimeLicensePage_I_do_not_accept_the_terms;
	public static String DownloadRuntimeLicensePage_Runtime;
	public static String DownloadRuntimeLicensePage_Runtime_License;
	public static String DownloadRuntimeLicensePage_This_license_must_be_accepted;
	public static String DownloadRuntimesSecondPage_19;
	public static String DownloadRuntimesSecondPage_Browse;
	public static String DownloadRuntimesSecondPage_Delete_archive_after_installing;
	public static String DownloadRuntimesSecondPage_Download_folder;
	public static String DownloadRuntimesSecondPage_Download_folder_is_required;
	public static String DownloadRuntimesSecondPage_Download_Runtime;
	public static String DownloadRuntimesSecondPage_Extracting;
	public static String DownloadRuntimesSecondPage_Install_folder;
	public static String DownloadRuntimesSecondPage_Install_folder_does_not_exist;
	public static String DownloadRuntimesSecondPage_Install_folder_is_required;
	public static String DownloadRuntimesSecondPage_No_runtime_server_found;
	public static String DownloadRuntimesSecondPage_Question;
	public static String DownloadRuntimesSecondPage_Select_install_folder;
	public static String DownloadRuntimesSecondPage_Select_ownload_folder;
	public static String DownloadRuntimesSecondPage_The_file_already_exists;
	public static String DownloadRuntimesSecondPage_This_folder_does_not_exist;
	public static String DownloadRuntimesSecondPage_This_folder_is_required;
	public static String DownloadRuntimesSecondPage_URL;
	public static String DownloadRuntimesSecondPage_URL_is_not_valid;
	public static String DownloadRuntimesSecondPage_Warning;
	public static String DownloadRuntimesWizard_Download_Runtimes;
	public static String DownloadRuntimesWizardFirstPage_Download_Runtimes;
	public static String DownloadRuntimesWizardFirstPage_Name;
	public static String DownloadRuntimesWizardFirstPage_No_available_runtime;
	public static String DownloadRuntimesWizardFirstPage_Please_select_a_runtime;
	public static String DownloadRuntimesWizardFirstPage_URL;
	public static String DownloadRuntimesWizardFirstPage_Version;
	public static String DownloadRuntimeViewerDialog_Download_and_Install_Runtime;
	public static String DownloadRuntimeViewerDialog_Download_Runtimes;
	public static String DownloadRuntimeViewerDialog_Name;
	public static String DownloadRuntimeViewerDialog_Please_select_a_runtime;
	public static String DownloadRuntimeViewerDialog_URL;
	public static String DownloadRuntimeViewerDialog_Version;
	public static String EditRuntimePathDialog_Browse;
	public static String EditRuntimePathDialog_Edit_Path;
	public static String EditRuntimePathDialog_Edit_runtime_detection_path;
	public static String EditRuntimePathDialog_Path;
	public static String EditRuntimePathDialog_Refresh;
	public static String EditRuntimePathDialog_Runtimes_found_at_this_path;
	public static String RuntimeCheckboxTreeViewer_Location;
	public static String RuntimeCheckboxTreeViewer_Name;
	public static String RuntimeCheckboxTreeViewer_Type;
	public static String RuntimeCheckboxTreeViewer_Version;
	public static String RuntimePreferencePage_A_Link_a;
	public static String RuntimePreferencePage_Add;
	public static String RuntimePreferencePage_Add_a_new_path;
	public static String RuntimePreferencePage_Add_Runtime_Path;
	public static String RuntimePreferencePage_Available_runtime_detectors;
	public static String RuntimePreferencePage_Description;
	public static String RuntimePreferencePage_Detector_is_invalid;
	public static String RuntimePreferencePage_Download;
	public static String RuntimePreferencePage_Each_path_on_this_list;
	public static String RuntimePreferencePage_Edit;
	public static String RuntimePreferencePage_Edit_Runtime_path;
	public static String RuntimePreferencePage_Every_start;
	public static String RuntimePreferencePage_Information;
	public static String RuntimePreferencePage_Link;
	public static String RuntimePreferencePage_Open_download_wizard;
	public static String RuntimePreferencePage_Open_preferences;
	public static String RuntimePreferencePage_Path;
	public static String RuntimePreferencePage_Paths;
	public static String RuntimePreferencePage_Remove;
	public static String RuntimePreferencePage_Search;
	public static String RuntimePreferencePage_This_runtime_path_already_exists;
	public static String RuntimePreferencePage_Type;
	public static String RuntimePreferencePage_You_have_unsaved_changes;
	public static String RuntimePreferencePage_You_have_unsaved_changes2;
	public static String RuntimeScanner_JBoss_Runtime_Detector_checking;
	public static String RuntimeScanner_Searching_runtimes;
	public static String SearchRuntimePathDialog_Deselect_All;
	public static String SearchRuntimePathDialog_Hide_already_created_runtimes;
	public static String SearchRuntimePathDialog_Initializing_runtimes;
	public static String SearchRuntimePathDialog_New_runtime_found;
	public static String SearchRuntimePathDialog_New_runtimes_found;
	public static String SearchRuntimePathDialog_No_runtime_found;
	public static String SearchRuntimePathDialog_Searching;
	public static String SearchRuntimePathDialog_Searching_for_runtimes;
	public static String SearchRuntimePathDialog_Searching_runtimes_is_canceled;
	public static String SearchRuntimePathDialog_Searching_runtimes_is_finished;
	public static String SearchRuntimePathDialog_Select_All;
	public static String MissingAuthenticatorUI;
	public static String WizardError;
	
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
