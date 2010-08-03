/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 

package org.jboss.tools.common.ui.preferences;

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.internal.ui.preferences.PropertyAndPreferencePage;
import org.eclipse.jface.dialogs.ControlEnableState;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.jboss.tools.common.preferences.SeverityPreferences;

/**
 * @author Viacheslav Kabanovich
 */
public abstract class SeverityPreferencePage extends PropertyAndPreferencePage {

	private ControlEnableState mainBlockEnableState;
	private Button checkBox;
	protected Control severityConfigurationBlock;

	protected SeverityConfigurationBlock fConfigurationBlock;

	protected SeverityConfigurationBlock getConfigurationBlock() {
		return fConfigurationBlock;
	}

	@Override
	protected Control createPreferenceContent(Composite composite) {
		Composite root = new Composite(composite, SWT.NONE);

		GridData gd = new GridData(GridData.FILL, GridData.FILL, true, true);

		GridLayout gridLayout = new GridLayout(1, false);
		root.setLayout(gridLayout);
		root.setLayoutData(gd);

		checkBox = new Button(root, SWT.CHECK);
		checkBox.setFont(JFaceResources.getDialogFont());
		checkBox.setText(SeverityPreferencesMessages.ENABLE_VALIDATION);
		checkBox.setSelection(getPreferenceStore().getBoolean(SeverityPreferences.ENABLE_BLOCK_PREFERENCE_NAME));

		severityConfigurationBlock = getConfigurationBlock().createContents(root);
		gridLayout = new GridLayout(1, false);
		severityConfigurationBlock.setLayoutData(gd);

		checkBox.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				enableMainPreferenceContent(checkBox.getSelection());
				getPreferenceStore().setValue(SeverityPreferences.ENABLE_BLOCK_PREFERENCE_NAME, checkBox.getSelection());
			}
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
		enableMainPreferenceContent(checkBox.getSelection());

		return root;
	}

	protected void enableMainPreferenceContent(boolean enable) {
		if (enable) {
			if (mainBlockEnableState != null) {
				mainBlockEnableState.restore();
				mainBlockEnableState= null;
			}
		} else {
			if (mainBlockEnableState == null) {
				mainBlockEnableState= ControlEnableState.disable(severityConfigurationBlock);
			}
		}
	}

	@Override
	protected boolean hasProjectSpecificOptions(IProject project) {
		return getConfigurationBlock().hasProjectSpecificOptions(project);
	}

	/*
	 * @see org.eclipse.jface.dialogs.DialogPage#dispose()
	 */
	@Override
	public void dispose() {
		if (getConfigurationBlock() != null) {
			getConfigurationBlock().dispose();
		}
		super.dispose();
	}

	/*
	 * @see org.eclipse.jdt.internal.ui.preferences.PropertyAndPreferencePage#enableProjectSpecificSettings(boolean)
	 */
	@Override
	protected void enableProjectSpecificSettings(boolean useProjectSpecificSettings) {
		super.enableProjectSpecificSettings(useProjectSpecificSettings);
		if (getConfigurationBlock() != null) {
			getConfigurationBlock().useProjectSpecificSettings(useProjectSpecificSettings);
		}
	}

	/*
	 * @see org.eclipse.jface.preference.IPreferencePage#performDefaults()
	 */
	@Override
	protected void performDefaults() {
		checkBox.setSelection(true);
		enableMainPreferenceContent(true);
		super.performDefaults();
		if (getConfigurationBlock() != null) {
			getConfigurationBlock().performDefaults();
		}
	}

	/*
	 * @see org.eclipse.jface.preference.IPreferencePage#performOk()
	 */
	@Override
	public boolean performOk() {
		if (getConfigurationBlock() != null && !getConfigurationBlock().performOk()) {
			return false;
		}
		updateEnableBlock();
		return super.performOk();
	}

	/*
	 * @see org.eclipse.jface.preference.IPreferencePage#performApply()
	 */
	@Override
	public void performApply() {
		if (getConfigurationBlock() != null) {
			getConfigurationBlock().performApply();
		}
		updateEnableBlock();
	}

	private void updateEnableBlock() {
		boolean oldValue = getPreferenceStore().getBoolean(SeverityPreferences.ENABLE_BLOCK_PREFERENCE_NAME);
		boolean newValue = checkBox.getSelection();
		if(oldValue != newValue) {
			getPreferenceStore().setValue(SeverityPreferences.ENABLE_BLOCK_PREFERENCE_NAME, newValue);
		}
	}
}