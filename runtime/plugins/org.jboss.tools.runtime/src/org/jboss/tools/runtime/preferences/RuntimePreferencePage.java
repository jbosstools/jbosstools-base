/*************************************************************************************
 * Copyright (c) 2008-2009 JBoss by Red Hat and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.preferences;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.layout.PixelConverter;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.jboss.tools.runtime.Activator;
import org.jboss.tools.runtime.JBossRuntimeLocator;

/**
 * @author Snjeza
 * 
 */
public class RuntimePreferencePage extends PreferencePage implements
		IWorkbenchPreferencePage {

	private static final String LASTPATH = "lastPath";
	public static final String SEAM_PREFERENCES_ID = "org.jboss.tools.common.model.ui.seam"; //$NON-NLS-1$
	public static final String WTP_PREFERENCES_ID = "org.eclipse.wst.server.ui.runtime.preferencePage"; //$NON-NLS-1$
	private static final String DROOLS_PREFERENCES_ID = "org.drools.eclipse.preferences.DroolsRuntimesPreferencePage";
	private static final String JBPM_PREFERENCES_ID = "org.jboss.tools.jbpm.locations";
	

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse
	 * .swt.widgets.Composite)
	 */
	@Override
	protected Control createContents(Composite parent) {
		noDefaultAndApplyButton();
		initializeDialogUnits(parent);
		
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(1, false);
		
		//layout.marginHeight= convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_MARGIN);
		//layout.marginWidth= 0;
		//layout.verticalSpacing= convertVerticalDLUsToPixels(10);
		//layout.horizontalSpacing= convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_SPACING);
		composite.setLayout(layout);
		
		createLink(composite, "See <a>WTP Runtime</a>", WTP_PREFERENCES_ID);
		createLink(composite, "See <a>Seam Runtime</a>", SEAM_PREFERENCES_ID);
		createLink(composite, "See <a>Drools Runtime</a>", DROOLS_PREFERENCES_ID);
		createLink(composite, "See <a>JBPM Runtime</a>", JBPM_PREFERENCES_ID);
		
		new Label(composite, SWT.NONE);
		
		Label searchLabel = new Label(composite, SWT.NONE);
		searchLabel.setText("JBoss AS, Seam, JBPM and Drools Runtimes wiil be configured based on the entered path.\nYou can also export/import configured JBoss runtimes.");
		
		Group runtimeGroup = new Group(composite, SWT.NONE);
		layout = new GridLayout(1, false);
		layout.marginWidth = 0;
		layout.marginHeight = 0;
		runtimeGroup.setLayout(layout);
		runtimeGroup.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		runtimeGroup.setText("JBoss Runtimes");
		Button searchButton = createButton(runtimeGroup, "Search for JBoss Runtimes", "Search...");
		searchButton.addSelectionListener(new SelectionAdapter() {
			
			public void widgetSelected(SelectionEvent e) {
				search();
			}

		});
		
		Button exportButton = createButton(runtimeGroup, "Export JBoss Runtimes", "Export...");
		exportButton.addSelectionListener(new SelectionAdapter() {
			
			public void widgetSelected(SelectionEvent e) {
				exportRuntimes();
			}

		});
		
		Button importButton = createButton(runtimeGroup, "Import JBoss Runtimes", "Import...");
		importButton.addSelectionListener(new SelectionAdapter() {
			
			public void widgetSelected(SelectionEvent e) {
				importRuntimes();
			}

		});
		
		setButtonDimensionHint(searchButton);
		setButtonDimensionHint(exportButton);
		setButtonDimensionHint(importButton);
		Dialog.applyDialogFont(composite);
		return composite;
	}

	private void exportRuntimes() {
		// TODO Auto-generated method stub	
	}
	
	private void importRuntimes() {
		// TODO Auto-generated method stub	
	}
	
	private Button createButton(Composite parent, String labelText, String buttonText) {
		GridLayout layout;
		Composite composite = new Composite(parent,SWT.NONE);	
		layout = new GridLayout(2, false);
		layout.marginWidth = 0;
		layout.marginHeight = 0;
		composite.setLayout(layout);
		composite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		Label label = new Label(composite, SWT.NONE);
		label.setText(labelText);
		GridData data= new GridData(GridData.FILL, GridData.CENTER, true, false);
		data.widthHint= convertVerticalDLUsToPixels(50);
		label.setLayoutData(data);
		
		Button button = new Button(composite, SWT.PUSH);
		button.setText(buttonText);
		data = new GridData(GridData.FILL, GridData.BEGINNING, false, false);
		//data.widthHint= convertVerticalDLUsToPixels(60);
		button.setLayoutData(data);
		return button;
	}

	private void createLink(Composite composite, String text, final String preferencesId) {
		Link wtpRuntime = new Link(composite, SWT.NONE);
		wtpRuntime.setLayoutData(new GridData(SWT.BEGINNING, SWT.CENTER, false, false));
		wtpRuntime.setText(text);
		wtpRuntime.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				PreferenceDialog dialog = PreferencesUtil.createPreferenceDialogOn(getShell(),preferencesId, new String[] {preferencesId},null);
				if (dialog != null) {
					dialog.open();
				}
			}
		});
	}

	public void init(IWorkbench workbench) {
	}
	
	private void search() {
		IDialogSettings dialogSettings = Activator.getDefault().getDialogSettings();
		String lastUsedPath= dialogSettings.get(LASTPATH);
		if (lastUsedPath == null) {
			lastUsedPath= ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString();
		}
		DirectoryDialog dialog = new DirectoryDialog(getShell());
		dialog.setMessage("Search for JBoss Runtimes");
		dialog.setFilterPath(lastUsedPath);
		String path = dialog.open();
		if (path == null) {
			return;
		}
		dialogSettings.put(LASTPATH, path);
		JBossRuntimeLocator locator = new JBossRuntimeLocator();
		locator.searchForRuntimes(path, new NullProgressMonitor());
	}
	
	/**
	 * Returns a width hint for a button control.
	 * @param button the button
	 * @return the width hint
	 */
	public static int getButtonWidthHint(Button button) {
		button.setFont(JFaceResources.getDialogFont());
		PixelConverter converter= new PixelConverter(button);
		int widthHint= converter.convertHorizontalDLUsToPixels(IDialogConstants.BUTTON_WIDTH);
		return Math.max(widthHint, button.computeSize(SWT.DEFAULT, SWT.DEFAULT, true).x);
	}

	/**
	 * Sets width and height hint for the button control.
	 * <b>Note:</b> This is a NOP if the button's layout data is not
	 * an instance of <code>GridData</code>.
	 *
	 * @param button	the button for which to set the dimension hint
	 */
	public static void setButtonDimensionHint(Button button) {
		Assert.isNotNull(button);
		Object gd= button.getLayoutData();
		if (gd instanceof GridData) {
			((GridData)gd).widthHint= getButtonWidthHint(button);
			((GridData)gd).horizontalAlignment = GridData.FILL;
		}
	}

}
