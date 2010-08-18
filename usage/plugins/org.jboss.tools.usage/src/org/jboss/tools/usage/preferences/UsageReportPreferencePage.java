/*******************************************************************************
 * Copyright (c) 2008 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.preferences;

import java.io.IOException;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.preferences.ConfigurationScope;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IPersistentPreferenceStore;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.preferences.ScopedPreferenceStore;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.util.StatusUtils;

/**
 * @author Andre Dietisheim
 */
public class UsageReportPreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

	private IPersistentPreferenceStore preferences;

	public UsageReportPreferencePage() {
		super(GRID);
		setPreferenceStore(this.preferences = createPreferencesStore());
	}

	private IPersistentPreferenceStore createPreferencesStore() {
		return new ScopedPreferenceStore(
				new ConfigurationScope(),
				JBossToolsUsageActivator.PLUGIN_ID);
	}

	public void createFieldEditors() {
		addField(new BooleanFieldEditor(
				IUsageReportPreferenceConstants.USAGEREPORT_ENABLED
				, "&Allow JBoss Tools to report usage analytics anonymously for statistical matters"
				, getFieldEditorParent()));
	}

	public void init(IWorkbench workbench) {
	}

	@Override
	public boolean performOk() {
		try {
			preferences.save();
		} catch (IOException e) {
			IStatus status = StatusUtils.getErrorStatus(JBossToolsUsageActivator.PLUGIN_ID,
					"Could not save the preferences.", e);
			JBossToolsUsageActivator.getDefault().getLog().log(status);
		}
		return super.performOk();
	}
}