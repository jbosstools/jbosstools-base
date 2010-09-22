/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.preferences;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.jboss.tools.usage.internal.JBDSUtils;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.util.StatusUtils;
import org.osgi.service.prefs.BackingStoreException;

/**
 * @author Andre Dietisheim
 */
public class UsageReportPreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

	public UsageReportPreferencePage() {
		super(GRID);
	}

	public void createFieldEditors() {
		addField(new BooleanFieldEditor(
				IUsageReportPreferenceConstants.USAGEREPORT_ENABLED_ID
				, getCheckBoxlabel()
				, getFieldEditorParent()));
	}

	private String getCheckBoxlabel() {
		if (JBDSUtils.isJBDS()) {
			return PreferencesMessages.UsageReportPreferencePage_AllowReporting_JBDS;
		} else {
			return PreferencesMessages.UsageReportPreferencePage_AllowReporting;
		}
	}
	

	public void init(IWorkbench workbench) {
		setPreferenceStore(UsageReportPreferences.createPreferenceStore());
		setDescription(getPageDescription());
	}

	private String getPageDescription() {
		if (JBDSUtils.isJBDS()) {
			return PreferencesMessages.UsageReportPreferencePage_Description_JBDS;
		} else {
			return PreferencesMessages.UsageReportPreferencePage_Description;
		}
	}

	@Override
	public boolean performOk() {
		try {
			UsageReportPreferences.flush();
		} catch (BackingStoreException e) {
			IStatus status = StatusUtils.getErrorStatus(JBossToolsUsageActivator.PLUGIN_ID,
					getPrefsSaveErrorMessage() , e);
			JBossToolsUsageActivator.getDefault().getLog().log(status);
		}
		return super.performOk();
	}

	private String getPrefsSaveErrorMessage() {
		if (JBDSUtils.isJBDS()) {
			return PreferencesMessages.UsageReportPreferencePage_Error_Saving_JBDS;
		} else {
			return PreferencesMessages.UsageReportPreferencePage_Error_Saving;
		}
	}
	
	
}