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

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.layout.PixelConverter;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
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
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.internal.wizards.preferences.PreferencesExportWizard;
import org.eclipse.ui.internal.wizards.preferences.PreferencesImportWizard;
import org.eclipse.wst.server.core.IServerWorkingCopy;
import org.eclipse.wst.server.core.internal.IMemento;
import org.eclipse.wst.server.core.internal.Server;
import org.eclipse.wst.server.core.internal.ServerPlugin;
import org.eclipse.wst.server.core.internal.XMLMemento;
import org.jboss.tools.jbpm.preferences.JbpmInstallation;
import org.jboss.tools.jbpm.preferences.PreferencesManager;
import org.jboss.tools.runtime.Activator;
import org.jboss.tools.runtime.JBossRuntimeLocator;
import org.jboss.tools.runtime.JBossRuntimeStartup;
import org.jboss.tools.runtime.ServerDefinition;

/**
 * @author Snjeza
 * 
 */
public class RuntimePreferencePage extends PreferencePage implements
		IWorkbenchPreferencePage {

	private static final String SERVER_DATA_FILE = "servers.xml";

	private static final String LASTPATH = "lastPath";
	public static final String SEAM_PREFERENCES_ID = "org.jboss.tools.common.model.ui.seam"; //$NON-NLS-1$
	public static final String WTP_PREFERENCES_ID = "org.eclipse.wst.server.ui.runtime.preferencePage"; //$NON-NLS-1$
	private static final String DROOLS_PREFERENCES_ID = "org.drools.eclipse.preferences.DroolsRuntimesPreferencePage";
	private static final String JBPM_PREFERENCES_ID = "org.jboss.tools.jbpm.locations";
	
	protected Map<String, Object> map = new HashMap<String, Object>();

	private List<ServerDefinition> serverDefinitions;

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
		exportServers();
		exportJbpms();
		PreferencesExportWizard wizard = new PreferencesExportWizard();
		wizard.init(PlatformUI.getWorkbench(), new StructuredSelection());
		WizardDialog dialog = new WizardDialog(getShell(), wizard);
		dialog.create();
		dialog.open();
	}
	
	private void exportJbpms() {
		File file = org.jboss.tools.jbpm.Activator.getDefault().getStateLocation().append("jbpm-installations.xml").toFile();
		if (!file.exists()) {
			Activator.getDefault().getPreferenceStore().setValue(Activator.JBPMS, "");
			return;
		}
		try {
			XMLMemento memento = (XMLMemento) XMLMemento.loadMemento(file.getAbsolutePath());
			String xmlString = memento.saveToString();
			Activator.getDefault().getPreferenceStore().setValue(Activator.JBPMS, xmlString);
			Activator.getDefault().savePluginPreferences();
		} catch (Exception e) {
			Activator.log (e);
		}
	}

	private void exportServers() {
		String filename = ServerPlugin.getInstance().getStateLocation().append(SERVER_DATA_FILE).toOSString();
		if ( !(new File(filename).exists()) ) {
			Activator.getDefault().getPreferenceStore().setValue(Activator.SERVERS, "");
			return;
		}
		try {
			XMLMemento memento = (XMLMemento) XMLMemento.loadMemento(filename);
			String xmlString = memento.saveToString();
			Activator.getDefault().getPreferenceStore().setValue(Activator.SERVERS, xmlString);
			Activator.getDefault().savePluginPreferences();
		} catch (Exception e) {
			Activator.log (e);
		}
	}

	private void importRuntimes() {
		PreferencesImportWizard wizard = new PreferencesImportWizard();
		wizard.init(PlatformUI.getWorkbench(), new StructuredSelection());
		WizardDialog dialog = new WizardDialog(getShell(), wizard);
		dialog.create();
		int ok = dialog.open();
		if (ok == Window.OK) {
			String jbpms = Activator.getDefault().getPreferenceStore().getString(Activator.JBPMS);
			if (jbpms != null && jbpms.trim().length() > 0) {
				loadJBPMInstalations(jbpms);
			}
			String servers = Activator.getDefault().getPreferenceStore().getString(Activator.SERVERS);
			if (servers != null && servers.trim().length() > 0) {
				loadServerInstalations(servers);
			}
		}
	}
	
	/**
	 * @param servers
	 */
	private void loadServerInstalations(String servers) {
		InputStream in = null;
		try {
			in = new ByteArrayInputStream(servers.getBytes("UTF-8"));
			IMemento memento = XMLMemento.loadMemento(in);
			
			IMemento[] children = memento.getChildren("server");
			int size = children.length;
			
			for (int i = 0; i < size; i++) {
				ServerEx server = new ServerEx(null);
				server.loadFromMemento(children[i], null);
				IServerWorkingCopy wc = server.createWorkingCopy();
				wc.save(false, null);
			}
		} catch (Exception e) {
			Activator.log(e);
		}
	}

	/**
	 * @param jbpms
	 */
	private void loadJBPMInstalations(String jbpms) {
		InputStream in = null;
		try {
			in = new ByteArrayInputStream(jbpms.getBytes("UTF-8"));
			IMemento memento = XMLMemento.loadMemento(in);
			IMemento[] children = memento.getChildren("installation");
			for (int i = 0; i < children.length; i++) {
				JbpmInstallation installation = new JbpmInstallation();
				installation.name = children[i].getString("name");
				installation.location = children[i].getString("location");
				installation.version = children[i].getString("version");
				PreferencesManager.getInstance().getJbpmInstallationMap()
						.put(installation.name, installation);
			}
		} catch (Exception e) {
			Activator.log(e);
		} finally {
			try {
				if (in != null) {
					in.close();
				}
			} catch (IOException e) {
			}
		}
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
		Link link = new Link(composite, SWT.NONE);
		link.setLayoutData(new GridData(SWT.BEGINNING, SWT.CENTER, false, false));
		link.setText(text);
		link.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				PreferencesUtil.createPreferenceDialogOn(getShell(),preferencesId, null, null);
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
		final String path = dialog.open();
		if (path == null) {
			return;
		}
		dialogSettings.put(LASTPATH, path);
		
		IRunnableWithProgress op = new IRunnableWithProgress() {
			
			public void run(IProgressMonitor monitor) {
				JBossRuntimeLocator locator = new JBossRuntimeLocator();
				serverDefinitions = locator.searchForRuntimes(path, monitor);
				
			}
		};
		
		try {
			serverDefinitions = null;
			new ProgressMonitorDialog(getShell()).run(true, true, op);
		} catch (InvocationTargetException e) {
			Activator.log(e);
		} catch (InterruptedException e) {
			//  ignore
		}

		if (serverDefinitions == null || serverDefinitions.size() == 0) {
			MessageDialog.openInformation(null, "Search for JBoss Runtimes",
							"The search found 0 runtimes while searching "
									+ path + ".");
		} else {
			RuntimeDialog runtimeDialog = new RuntimeDialog(getShell(),
					serverDefinitions, path);
			int ok = runtimeDialog.open();
			if (ok == Window.OK) {
				Iterator<ServerDefinition> iterator = serverDefinitions.iterator();
				while (iterator.hasNext()) {
					 ServerDefinition definition = iterator.next();
					 if (!definition.isEnabled()) {
						 iterator.remove();
					 }
				}
				JBossRuntimeStartup runtimeStartup = new JBossRuntimeStartup();
				runtimeStartup.initializeRuntimes(serverDefinitions);
			}
		}
			
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
	
	private static class ServerEx extends Server {

		/**
		 * @param file
		 */
		public ServerEx(IFile file) {
			super(file);
		}
		
		@Override
		public void loadFromMemento(IMemento memento, IProgressMonitor monitor) {
			super.loadFromMemento(memento, monitor);
		}
		
	}
}
