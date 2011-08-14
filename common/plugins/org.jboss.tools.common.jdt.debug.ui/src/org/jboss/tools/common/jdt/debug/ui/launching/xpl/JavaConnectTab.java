/*******************************************************************************
 * Copyright (c) 2000, 2010 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     JBoss by Red Hat
 *******************************************************************************/
package org.jboss.tools.common.jdt.debug.ui.launching.xpl;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.debug.ui.IJavaDebugHelpContextIds;
import org.eclipse.jdt.internal.debug.ui.JDIDebugUIPlugin;
import org.eclipse.jdt.internal.debug.ui.launcher.AbstractJavaMainTab;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IVMConnector;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.jboss.tools.common.jdt.debug.RemoteDebugActivator;
import org.jboss.tools.common.jdt.debug.VmModel;
import org.jboss.tools.common.jdt.debug.ui.Messages;
import org.jboss.tools.common.jdt.debug.ui.RemoteDebugUIActivator;
import org.jboss.tools.common.jdt.debug.ui.preferences.RemoteDebug;

/**
 * A launch configuration tab that displays and edits the project associated
 * with a remote connection and the connector used to connect to a remote
 * VM.
 * <p>
 * This class may be instantiated.
 * </p>
 * @since 2.0
 * @noextend This class is not intended to be subclassed by clients.
 */
public class JavaConnectTab extends AbstractJavaMainTab {
	
	private static final String HOST_NAMES = "hostNames"; //$NON-NLS-1$
	private static final String DEFAULT_HOST = "defaultHost"; //$NON-NLS-1$
	
	// UI widgets
	private Button fAllowTerminateButton;
	private Composite fArgumentComposite;
	private IVMConnector fConnector;
	private Combo hostCombo;
	private Combo portCombo;
	private String[] items;
	private VmModel[] models;
	private Button defaultButton;
	private Group messageGroup;
	
	private void updateConnector() {
		getSelectedConnector();
		IDialogSettings dialogSettings = RemoteDebugUIActivator.getDefault().getDialogSettings();
		String names = dialogSettings.get(HOST_NAMES);
		String[] hostNames;
		if (names == null) {
			names = RemoteDebugActivator.LOCALHOST;
			dialogSettings.put(HOST_NAMES, names);
			hostNames = new String[] {names};
		} else {
			StringTokenizer tokenizer = new StringTokenizer(names, ","); //$NON-NLS-1$
			Set<String> hostSets = new HashSet<String>();
			hostSets.add(RemoteDebugActivator.LOCALHOST);
			while (tokenizer.hasMoreTokens()) {
				hostSets.add(tokenizer.nextToken());
			}
			hostNames = hostSets.toArray(new String[0]);
		}
		// create editors
		// host
		Composite hostComposite = new Composite(fArgumentComposite, SWT.NONE);
		GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
		gd.horizontalSpan = 2;
		hostComposite.setLayoutData(gd);
		hostComposite.setLayout(new GridLayout(2, false));
		Label hostLabel = new Label(hostComposite, SWT.NONE);
		gd = new GridData(SWT.FILL, SWT.FILL, false, false);
		hostLabel.setLayoutData(gd);
		hostLabel.setText(Messages.JavaConnectTab_Host);
		// FIXME
		hostCombo = new Combo(hostComposite, SWT.READ_ONLY);
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		hostCombo.setLayoutData(gd);
		hostCombo.setItems(hostNames);
		String defaultHost = dialogSettings.get(DEFAULT_HOST);
		if (defaultHost == null) {
			defaultHost = RemoteDebugActivator.LOCALHOST;
			dialogSettings.put(DEFAULT_HOST, defaultHost);
		}
		hostCombo.setText(defaultHost);
		
		hostCombo.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				updateLaunchConfigurationDialog();
			}
		
		});
		
		// port
		
		Composite portComposite = new Composite(fArgumentComposite, SWT.NONE);
		gd = new GridData(SWT.FILL, SWT.FILL, true, true);
		gd.horizontalSpan = 2;
		portComposite.setLayoutData(gd);
		portComposite.setLayout(new GridLayout(3, false));
		Label portLabel = new Label(portComposite, SWT.NONE);
		gd = new GridData(SWT.FILL, SWT.FILL, false, false);
		portLabel.setLayoutData(gd);
		portLabel.setText(Messages.JavaConnectTab_Port);
		// FIXME
		portCombo = new Combo(portComposite, SWT.READ_ONLY);
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		portCombo.setLayoutData(gd);
		refresh(defaultHost, false);
		
		portCombo.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				updateLaunchConfigurationDialog();
			}
		
		});
		
		Button refreshButton = new Button(portComposite, SWT.PUSH);
		gd = new GridData(SWT.FILL, SWT.FILL, false, false);
		refreshButton.setLayoutData(gd);
		refreshButton.setText(Messages.JavaConnectTab_Refresh);
		refreshButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				refresh(hostCombo.getText(), true);
				updateLaunchConfigurationDialog();
			}
			
		});
		updateLaunchConfigurationDialog();		
	}

	private void refresh(String defaultHost, boolean discover) {
		if (!discover) {
			boolean oldDiscoverPreferences = RemoteDebugUIActivator.getDefault().isDiscoverRemoteApplication();
			RemoteDebugUIActivator.getDefault().getPreferences().putBoolean(RemoteDebugUIActivator.DISCOVER_REMOTE_APPLICATION, false);
			models = RemoteDebugUIActivator.getDefault().getDebugModels(new NullProgressMonitor());
			RemoteDebugUIActivator.getDefault().getPreferences().putBoolean(RemoteDebugUIActivator.DISCOVER_REMOTE_APPLICATION, oldDiscoverPreferences);
		} else {
			RemoteDebugUIActivator.getDefault()
					.discoverRemoteApplication(new NullProgressMonitor());
			models = RemoteDebugUIActivator.getDefault().getDebugModels(new NullProgressMonitor());
		}
		refreshCombo();
	}

	private void refreshCombo() {
		items = new String[models.length];
		int i = 0;
		for (VmModel model:models) {
			List<RemoteDebug> remoteDebugs = RemoteDebugUIActivator.getDefault()
					.getValidRemoteDebugs();
			RemoteDebug remoteDebug = RemoteDebugUIActivator.findRemoteDebug(remoteDebugs,
					model.getPort());
			String displayText;
			if (remoteDebug != null) {
				if (remoteDebug.isShow()) {
					StringBuffer buffer = new StringBuffer();
					if (remoteDebug.getDescription() != null
							&& !remoteDebug.getDescription().isEmpty()) {
						if (buffer.length() > 0) {
							buffer.append(","); //$NON-NLS-1$
						}
						buffer.append(remoteDebug.getDescription());
					}
					String text = model.getMainClass();
					if (text != null
							&& !RemoteDebugActivator.UNKNOWN.equals(text)) {
						if (buffer.length() > 0) {
							buffer.append(",main="); //$NON-NLS-1$
						}
						buffer.append(text);
					}
					String pid = model.getPid();
					if (pid != null) {
						if (buffer.length() > 0) {
							buffer.append(",pid="); //$NON-NLS-1$
						}
						buffer.append(pid);
					}
					String port = model.getPort();
					if (port != null) {
						if (buffer.length() > 0) {
							buffer.append(",port="); //$NON-NLS-1$
						}
						buffer.append(port);
					}
					displayText = buffer.toString();
				} else {
					displayText = model.getDisplayName();
				}
			} else {
				displayText = model.getDisplayName();
			}
			items[i++] = displayText;
		}
		Display.getDefault().syncExec(new Runnable() {
			
			@Override
			public void run() {
				portCombo.setItems(items);
				if (items.length > 0) {
					portCombo.select(0);
					portCombo.setText(items[0]);
				}
			}
		});
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.debug.ui.ILaunchConfigurationTab#createControl(org.eclipse.swt.widgets.Composite)
	 */
	public void createControl(Composite parent) {
		Font font = parent.getFont();
		Composite comp = SWTFactory.createComposite(parent, font, 1, 1, GridData.FILL_BOTH);
		GridLayout layout = new GridLayout();
		layout.verticalSpacing = 0;
		comp.setLayout(layout);
		createProjectEditor(comp);
		createVerticalSpacer(comp, 1);
			
	//connection properties
		Group group = SWTFactory.createGroup(comp, Messages.JavaConnectTab_Connection_Properties_1, 2, 1, GridData.FILL_HORIZONTAL);
		Composite cgroup = SWTFactory.createComposite(group, font, 2, 1, GridData.FILL_HORIZONTAL);
		fArgumentComposite = cgroup;
		updateConnector();
		
		createVerticalSpacer(comp, 2);
		fAllowTerminateButton = createCheckButton(comp, Messages.JavaConnectTab__Allow_termination_of_remote_VM_6); 
		fAllowTerminateButton.addSelectionListener(getDefaultListener());
		
		createVerticalSpacer(comp, 2);
		defaultButton = createCheckButton(comp, Messages.JavaConnectTab_SetAsDefault);
		defaultButton.addSelectionListener(getDefaultListener());
		
		createVerticalSpacer(comp, 2);
		messageGroup = new Group(comp, SWT.NONE);
		messageGroup.setText(Messages.JavaConnectTab_Warning);
		messageGroup.setLayout(new GridLayout(3, false));
		GridData gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		messageGroup.setLayoutData(gd);
		
		Label noteLabel = new Label(messageGroup,SWT.NONE);
		gd=new GridData(SWT.BEGINNING, SWT.BEGINNING, false, false);
		noteLabel.setLayoutData(gd);
		Image image = JFaceResources.getImage(Dialog.DLG_IMG_MESSAGE_WARNING);
		image.setBackground(noteLabel.getBackground());
		noteLabel.setImage(image);
		
		Text noteText = new Text(messageGroup, SWT.WRAP | SWT.READ_ONLY);
		gd = new GridData(SWT.FILL, SWT.FILL, true, false);
		GC gc = new GC(parent);
		gd.heightHint = Dialog.convertHeightInCharsToPixels(gc
				.getFontMetrics(), 3);
		gc.dispose(); 
		noteText.setLayoutData(gd);
		noteText.setText(Messages.JavaConnectTab_JDK_Required);
		
		messageGroup.setVisible(!RemoteDebugActivator.getDefault().isJdk());
		
		Button addJDK = new Button(messageGroup, SWT.PUSH);
		addJDK.setText(Messages.JavaConnectTab_Add_JDK);
		gd=new GridData(SWT.BEGINNING, SWT.CENTER, false, false);
		addJDK.setLayoutData(gd);
		addJDK.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				String preferenceId = "org.eclipse.jdt.debug.ui.preferences.VMPreferencePage"; //$NON-NLS-1$
				PreferenceDialog dialog = PreferencesUtil.createPreferenceDialogOn(getShell(),preferenceId, null, null);
				dialog.open();
				refresh(hostCombo.getText(), false);
				updateLaunchConfigurationDialog();
				messageGroup.setVisible(!RemoteDebugActivator.getDefault().isJdk());
			}
		});
		
		
		setControl(comp);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(getControl(), IJavaDebugHelpContextIds.LAUNCH_CONFIGURATION_DIALOG_CONNECT_TAB);
	}
	
	 /* (non-Javadoc)
	 * @see org.eclipse.jdt.internal.debug.ui.launcher.AbstractJavaMainTab#initializeFrom(org.eclipse.debug.core.ILaunchConfiguration)
	 */
	public void initializeFrom(ILaunchConfiguration config) {
		super.initializeFrom(config);
		updateAllowTerminateFromConfig(config);
		updateSetAsDefaultFromConfig(config);
	}
	
	private void updateSetAsDefaultFromConfig(ILaunchConfiguration config) {
		ILaunchConfiguration[] configs = RemoteDebugActivator.getDefault().getLaunchConfigurations();
		if (configs != null && configs.length == 1) {
			if (config instanceof ILaunchConfigurationWorkingCopy) {
				ILaunchConfigurationWorkingCopy wc = (ILaunchConfigurationWorkingCopy) config;
				try {
					boolean isDefault = config.getAttribute(RemoteDebugActivator.SET_AS_DEFAULT, false);
					if (!isDefault) {
						wc.setAttribute(RemoteDebugActivator.SET_AS_DEFAULT, true);
						wc.doSave();
					}
				} catch (CoreException e) {
					// ignore
				}
			}
		}
		boolean isDefault = false;
		try {
			isDefault = config.getAttribute(RemoteDebugActivator.SET_AS_DEFAULT, false);	
		}
		catch (CoreException ce) {
			JDIDebugUIPlugin.log(ce);
		}
		defaultButton.setSelection(isDefault);	
	}
	
	/**
	 * Updates the state of the allow terminate check button from the specified configuration
	 * @param config the config to load from
	 */
	private void updateAllowTerminateFromConfig(ILaunchConfiguration config) {
		boolean allowTerminate = false;
		try {
			allowTerminate = config.getAttribute(IJavaLaunchConfigurationConstants.ATTR_ALLOW_TERMINATE, false);	
		}
		catch (CoreException ce) {JDIDebugUIPlugin.log(ce);}
		fAllowTerminateButton.setSelection(allowTerminate);	
	}

	
	/* (non-Javadoc)
	 * @see org.eclipse.debug.ui.ILaunchConfigurationTab#performApply(org.eclipse.debug.core.ILaunchConfigurationWorkingCopy)
	 */
	public void performApply(ILaunchConfigurationWorkingCopy config) {
		config.setAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, fProjText.getText().trim());
		config.setAttribute(IJavaLaunchConfigurationConstants.ATTR_ALLOW_TERMINATE, fAllowTerminateButton.getSelection());
		config.setAttribute(RemoteDebugActivator.SET_AS_DEFAULT, defaultButton.getSelection());
		config.setAttribute(IJavaLaunchConfigurationConstants.ATTR_VM_CONNECTOR, getSelectedConnector().getIdentifier());
		mapResources(config);
		Map attrMap = new HashMap(2);
		attrMap.put("hostname", hostCombo.getText()); //$NON-NLS-1$
		String port = getPort();
		if (port != null) {
			attrMap.put("port", port); //$NON-NLS-1$
		} else {
			attrMap.put("port", ""); //$NON-NLS-1$ //$NON-NLS-2$
		}
		config.setAttribute(IJavaLaunchConfigurationConstants.ATTR_CONNECT_MAP, attrMap);
		if (defaultButton.getSelection()) {
			ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
			ILaunchConfigurationType type = manager.getLaunchConfigurationType(RemoteDebugActivator.LAUNCH_CONFIGURATION_ID);
			ILaunchConfiguration[] configs = null;
			try {
				configs = manager.getLaunchConfigurations(type);
			} catch (CoreException e) {
				return;
			}
			for (ILaunchConfiguration configuration:configs) {
				try {
					boolean isDefault = configuration.getAttribute(RemoteDebugActivator.SET_AS_DEFAULT, false);
					if (isDefault && !config.getName().equals(configuration.getName())) {
						ILaunchConfigurationWorkingCopy wc = configuration.getWorkingCopy();
						wc.setAttribute(RemoteDebugActivator.SET_AS_DEFAULT, false);
						wc.doSave();
					}
				} catch (CoreException e) {
					RemoteDebugUIActivator.log(e);
				}
			}
		}
	}

	private String getPort() {
		int portIndex = portCombo.getSelectionIndex();
		String port = null;
		if (models != null && models.length > portIndex && portIndex >= 0) {
			port = models[portIndex].getPort();
		}
		return port;
	}
	
	/**
	 * Initialize default settings for the given Java element
	 */
	private void initializeDefaults(IJavaElement javaElement, ILaunchConfigurationWorkingCopy config) {
		initializeJavaProject(javaElement, config);
		//initializeName(javaElement, config);
		initializeHardCodedDefaults(config);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.debug.ui.ILaunchConfigurationTab#setDefaults(org.eclipse.debug.core.ILaunchConfigurationWorkingCopy)
	 */
	public void setDefaults(ILaunchConfigurationWorkingCopy config) {
		IJavaElement javaElement = getContext();
		if (javaElement == null) {
			initializeHardCodedDefaults(config);
		} 
		else {
			initializeDefaults(javaElement, config);
		}
	}

	/**
	 * Find the first instance of a type, compilation unit, class file or project in the
	 * specified element's parental hierarchy, and use this as the default name.
	 */
	private void initializeName(IJavaElement javaElement, ILaunchConfigurationWorkingCopy config) {
		String name = EMPTY_STRING;
		try {
			IResource resource = javaElement.getUnderlyingResource();
			if (resource != null) {
				name = resource.getName();
				int index = name.lastIndexOf('.');
				if (index > 0) {
					name = name.substring(0, index);
				}
			} 
			else {
				name= javaElement.getElementName();
			}
			name = getLaunchConfigurationDialog().generateName(name);				
		}
		catch (JavaModelException jme) {JDIDebugUIPlugin.log(jme);}
		config.rename(name);
	}

	/**
	 * Initialize those attributes whose default values are independent of any context.
	 */
	private void initializeHardCodedDefaults(ILaunchConfigurationWorkingCopy config) {
		config.setAttribute(IJavaLaunchConfigurationConstants.ATTR_ALLOW_TERMINATE, false);
		config.setAttribute(RemoteDebugActivator.SET_AS_DEFAULT, false);
		config.setAttribute(IJavaLaunchConfigurationConstants.ATTR_VM_CONNECTOR, getSelectedConnector().getIdentifier());
	}

	 /* (non-Javadoc)
	 * @see org.eclipse.debug.ui.AbstractLaunchConfigurationTab#isValid(org.eclipse.debug.core.ILaunchConfiguration)
	 */
	public boolean isValid(ILaunchConfiguration config) {	
		setErrorMessage(null);
		setMessage(null);	
		String name = fProjText.getText().trim();
		if (name.length() > 0) {
			if (!ResourcesPlugin.getWorkspace().getRoot().getProject(name).exists()) {
				setErrorMessage(Messages.JavaConnectTab_Project_does_not_exist_14); 
				return false;
			}
		}
		if (hostCombo.getText().isEmpty()) {
			setErrorMessage(Messages.JavaConnectTab_Invalid_Host);
			return false;
		}
		String port = getPort();
		if (port == null || port.isEmpty()) {
			setErrorMessage(Messages.JavaConnectTab_Invalid_Port);
			return false;
		}
		try {
			new Integer(port);
		} catch (NumberFormatException e) {
			setErrorMessage(Messages.JavaConnectTab_Invalid_Port);
			return false;
		}
		
		return true;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.debug.ui.ILaunchConfigurationTab#getName()
	 */
	public String getName() {
		return Messages.JavaConnectTab_Conn_ect_20;
	}			

	/* (non-Javadoc)
	 * @see org.eclipse.debug.ui.AbstractLaunchConfigurationTab#getImage()
	 */
	public Image getImage() {
		return DebugUITools.getImage(IDebugUIConstants.IMG_LCL_DISCONNECT);
	}
		
	/**
	 * @see org.eclipse.debug.ui.AbstractLaunchConfigurationTab#getId()
	 * 
	 * @since 3.3
	 */
	public String getId() {
		return "org.jboss.tools.common.jdt.debug.ui.javaConnectTab"; //$NON-NLS-1$
	}
	
	/**
	 * Returns the selected connector
	 */
	private IVMConnector getSelectedConnector() {
		if (fConnector == null) {
			fConnector = RemoteDebugActivator.getDefaultVMConnector();
		}
		return fConnector;
	}

}
