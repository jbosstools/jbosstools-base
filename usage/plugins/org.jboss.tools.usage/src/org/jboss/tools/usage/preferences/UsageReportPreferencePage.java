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
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
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

	@Override
	protected Control createContents(Composite parent) {
		Control control = super.createContents(parent);
		appendReportingData((Composite) control);
		return control;
	}

	private void appendReportingData(Composite control) {
		Group group = new Group(control, SWT.NONE);
		group.setText(PreferencesMessages.UsageReportPreferencePage_ReportedValues);
		GridDataFactory.fillDefaults().grab(true, false).hint(SWT.FILL, 300).applyTo(group);
		FillLayout fillLayout = new FillLayout();
		fillLayout.marginHeight = 4;
		fillLayout.marginWidth = 8;
		group.setLayout(fillLayout);
		StyledText text = new StyledText(group, SWT.BORDER | SWT.V_SCROLL);
		text.setText("This is the StyledText widget\n...\n...\n...\n...\n...\n...\n\n...\n...\n...\n...\n...\n...\n\n...\n...\n...\n...\n...\n...\n\n...\n...\n...\n...\n...\n...\n\n...\n...\n...\n...\n...\n...\n\n...\n...\n...\n...\n...\n...\n...\n...\n...\n...\n...\n...\n...\n...\n...\n...\n...\n...\n...\n...\n...\n...\n...\n...\n...\n...\n...\n...\n....");
		text.setEditable(false);
	}

	public void createFieldEditors() {
		addField(new BooleanFieldEditor(
				IUsageReportPreferenceConstants.USAGEREPORT_ENABLED_ID
				, getCheckBoxlabel()
				, getFieldEditorParent()));

		addReportedValues();

	}

	private void addReportedValues() {
		// TODO Auto-generated method stub

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
					getPrefsSaveErrorMessage(), e);
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