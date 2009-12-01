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
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * @author Viacheslav Kabanovich
 */
public abstract class SeverityPreferencePage extends PropertyAndPreferencePage {

	protected SeverityConfigurationBlock fConfigurationBlock;

	protected SeverityConfigurationBlock getConfigurationBlock() {
		return fConfigurationBlock;
	}

	@Override
	protected Control createPreferenceContent(Composite composite) {
		return getConfigurationBlock().createContents(composite);
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
	}
}