/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.model.ui;

import org.eclipse.osgi.util.NLS;

public final class ModelUIMessages extends NLS {

	private static final String BUNDLE_NAME = "org.jboss.tools.common.model.ui.messages";//$NON-NLS-1$

	private ModelUIMessages() {
		// Do not instantiate
	}

	public static String DecoratorTextPreferencesPage_AddVariable;
	public static String DecoratorTextPreferencesPage_Decorator;
	public static String DecoratorTextPreferencesPage_DefaultLabel;
	public static String DecoratorTextPreferencesPage_Format;
	public static String DecoratorTextPreferencesPage_Preview;
	public static String DecoratorTextPreferencesPage_SelectVariable;
	public static String DecoratorTextPreferencesPage_Text;
	public static String ResourceLayoutManager_ERROR_RESOURCE_NULL;
	public static String ResourceLayoutManager_ERROR_CREATE_DOCUMENT;
	public static String ResourceLayoutManager_WARNING_HEAD_EMPTY;
	public static String ResourceLayoutManager_ERROR_ANOTHER_HEAD;
	public static String ResourceLayoutManager_ERROR_UNKNOWN_EXCEPTION;
	public static String Editor_Cut_label;
	public static String Editor_Cut_tooltip;
	public static String Editor_Cut_image;
	public static String Editor_Cut_description;
	public static String Editor_Copy_label;
	public static String Editor_Copy_tooltip;
	public static String Editor_Copy_image;
	public static String Editor_Copy_description;
	public static String Editor_Paste_label;
	public static String Editor_Paste_tooltip;
	public static String Editor_Paste_image;
	public static String Editor_Paste_description;
	public static String Editor_Delete_label;
	public static String Editor_Delete_tooltip;
	public static String Editor_Delete_image;
	public static String Editor_Delete_description;
	public static String MainPreferencePage_Description;
	public static String Form_CANNOT_LOAD_ATTRIBUTE;
	
	public static String AbstractQueryWizardView_Cancel;
	public static String AbstractQueryWizardView_Close;
	public static String AbstractQueryWizardView_Help;
	public static String AbstractQueryWizardView_OK;
	public static String BundleLanguagesEditor_Add;
	public static String BundleLanguagesEditor_Delete;
	public static String BundleLanguagesEditor_LanguageCountry;
	public static String CommandBar_Finish;
	public static String CommandBar_Next;
	public static String CommandBar_NextArrow;
	public static String CommandBar_OK;
	public static String CommandBar_Run;
	public static String HiddenFileSystemsWizardView_HideAllJars;
	public static String HiddenFileSystemsWizardView_ShowAllJars;
	public static String HiddenPaletteTabsWizardView_HideAll;
	public static String HiddenPaletteTabsWizardView_ShowAll;
	public static String MutableMultipleChoiceFieldEditor_DeselectAll;
	public static String MutableMultipleChoiceFieldEditor_New;
	public static String MutableMultipleChoiceFieldEditor_SelectAll;

	

	static {
		NLS.initializeMessages(BUNDLE_NAME, ModelUIMessages.class);
	}
}