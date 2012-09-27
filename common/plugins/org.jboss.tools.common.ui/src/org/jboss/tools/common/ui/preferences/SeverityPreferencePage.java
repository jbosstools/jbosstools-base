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
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.core.runtime.preferences.InstanceScope;
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
import org.eclipse.ui.preferences.IWorkbenchPreferenceContainer;
import org.eclipse.ui.preferences.IWorkingCopyManager;
import org.eclipse.ui.preferences.WorkingCopyManager;
import org.jboss.tools.common.preferences.SeverityPreferences;
import org.jboss.tools.common.ui.preferences.SeverityConfigurationBlock.OptionDescription;
import org.jboss.tools.common.ui.preferences.SeverityConfigurationBlock.SectionDescription;

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

		severityConfigurationBlock = getConfigurationBlock().createContents(root);

		IScopeContext[] lookupOrder;
		IProject project = getProject();
		if (project != null) {
			lookupOrder = new IScopeContext[] {
				new ProjectScope(project),
				InstanceScope.INSTANCE,
				DefaultScope.INSTANCE
			};
		} else {
			lookupOrder = new IScopeContext[] {
				InstanceScope.INSTANCE,
				DefaultScope.INSTANCE
			};
		}
		final IScopeContext context = lookupOrder[0];
		IWorkbenchPreferenceContainer container = getConfigurationBlock().getContainer();

		final IWorkingCopyManager manager;
		if (container == null) {
			manager = new WorkingCopyManager();
		} else {
			manager = container.getWorkingCopyManager();
		}
		String value = getStoredValue(lookupOrder, manager, getConfigurationBlock().getQualifier(), SeverityPreferences.ENABLE_BLOCK_PREFERENCE_NAME);
		boolean enabled = value==null?true:Boolean.parseBoolean(value);

//		checkBox.setSelection(getPreferenceStore().getBoolean(SeverityPreferences.ENABLE_BLOCK_PREFERENCE_NAME));
		checkBox.setSelection(enabled);

		gridLayout = new GridLayout(1, false);
		severityConfigurationBlock.setLayoutData(gd);

		checkBox.addSelectionListener(new SelectionListener() {
			public void widgetSelected(SelectionEvent e) {
				enableMainPreferenceContent(checkBox.getSelection());
//				getPreferenceStore().setValue(SeverityPreferences.ENABLE_BLOCK_PREFERENCE_NAME, checkBox.getSelection());
				setStoredValue(context, Boolean.toString(checkBox.getSelection()), manager, getConfigurationBlock().getQualifier(), SeverityPreferences.ENABLE_BLOCK_PREFERENCE_NAME);
			}
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
		enableMainPreferenceContent(checkBox.getSelection());

		return root;
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
