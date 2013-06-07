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
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.jdt.internal.ui.preferences.PropertyAndPreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.preferences.IWorkingCopyManager;
import org.jboss.tools.common.ui.preferences.SeverityConfigurationBlock.OptionDescription;
import org.jboss.tools.common.ui.preferences.SeverityConfigurationBlock.SectionDescription;

/**
 * @author Viacheslav Kabanovich
 */
public abstract class SeverityPreferencePage extends PropertyAndPreferencePage {

	protected Control severityConfigurationBlock;

	protected SeverityConfigurationBlock fConfigurationBlock;

	protected SeverityConfigurationBlock getConfigurationBlock() {
		return fConfigurationBlock;
	}

	@Override
	protected Control createPreferenceContent(Composite composite) {
		return getConfigurationBlock().createContents(composite);
	}

	private IEclipsePreferences getNode(IScopeContext context, IWorkingCopyManager manager, String qualifier) {
		IEclipsePreferences node= context.getNode(qualifier);
		if (manager != null) {
			return manager.getWorkingCopy(node);
		}
		return node;
	}

	public String getStoredValue(IScopeContext context, IWorkingCopyManager manager, String qualifier, String key) {
		return getNode(context, manager, qualifier).get(key, null);
	}

	public String getStoredValue(IScopeContext[] lookupOrder, IWorkingCopyManager manager, String qualifier, String key) {
		for (int i = 0; i < lookupOrder.length; i++) {
			String value= getStoredValue(lookupOrder[i], manager, qualifier, key);
			if (value != null) {
				return value;
			}
		}
		return null;
	}

	public void setStoredValue(IScopeContext context, String value, IWorkingCopyManager manager, String qualifier, String key) {
		if (value != null) {
			getNode(context, manager, qualifier).put(key, value);
		} else {
			getNode(context, manager, qualifier).remove(key);
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

	public String getLabel(String preferenceId){
		for(SectionDescription section : getAllSections()){
			String label = getLabel(section, preferenceId);
			if(label != null){
				return label;
			}
		}
		return "";
	}
	
	private String getLabel(SectionDescription section, String preferenceId){
		for(OptionDescription option : section.getOptions()){
			if(option.key.getName().equals(preferenceId)){
				return option.label;
			}
		}
		for(SectionDescription s : section.getSections()){
			String label = getLabel(s, preferenceId);
			if(label != null){
				return label;
			}
		}
		return null;
	}
	
	protected abstract SectionDescription[] getAllSections();
	
	@Override
	public void applyData(Object data) {
		if(data instanceof String){
			getConfigurationBlock().doFilter((String)data);
		}else{
			super.applyData(data);
		}
	}

	/**
	 * Used by test.
	 * @return
	 */
	public String getFilterText() {
		return getConfigurationBlock().getFilterControl().getText();
	}
}