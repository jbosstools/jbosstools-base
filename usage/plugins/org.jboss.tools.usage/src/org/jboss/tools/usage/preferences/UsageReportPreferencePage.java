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
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.util.PreferencesUtil;
import org.jboss.tools.usage.util.StatusUtils;
import org.osgi.service.prefs.BackingStoreException;

/**
 * @author Andre Dietisheim
 */
public class UsageReportPreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

	public UsageReportPreferencePage() {
		super(GRID);
		setPreferenceStore(PreferencesUtil.createConfigurationPreferencesStore());
	}

	public void createFieldEditors() {
		addField(new BooleanFieldEditor(
				IUsageReportPreferenceConstants.USAGEREPORT_ENABLED_ID
				, Messages.UsageReportPreferencePage_AllowReporting
				, getFieldEditorParent()));
	}

	public void init(IWorkbench workbench) {
	}

	@Override
	public boolean performOk() {
		try {
			PreferencesUtil.getConfigurationPreferences().flush();
		} catch (BackingStoreException e) {
			IStatus status = StatusUtils.getErrorStatus(JBossToolsUsageActivator.PLUGIN_ID,
					Messages.UsageReportPreferencePage_Error_Saving, e);
			JBossToolsUsageActivator.getDefault().getLog().log(status);
		}
		return super.performOk();
	}
}