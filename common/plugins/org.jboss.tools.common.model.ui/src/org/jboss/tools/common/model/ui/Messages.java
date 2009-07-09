package org.jboss.tools.common.model.ui;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.jboss.tools.common.model.ui.messages"; //$NON-NLS-1$
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
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
