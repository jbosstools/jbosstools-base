/*************************************************************************************
 * Copyright (c) 2008-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.debug.ui.preferences;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationListener;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.internal.ui.DebugPluginImages;
import org.eclipse.debug.internal.ui.DebugUIPlugin;
import org.eclipse.debug.internal.ui.launchConfigurations.LaunchConfigurationManager;
import org.eclipse.debug.internal.ui.launchConfigurations.LaunchConfigurationsDialog;
import org.eclipse.debug.internal.ui.launchConfigurations.LaunchGroupExtension;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.jdt.launching.IVMInstall;
import org.eclipse.jdt.launching.IVMInstallChangedListener;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jdt.launching.PropertyChangeEvent;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.jboss.tools.common.jdt.debug.IPropertyKeys;
import org.jboss.tools.common.jdt.debug.RemoteDebugActivator;
import org.jboss.tools.common.jdt.debug.tools.ToolsCore;
import org.jboss.tools.common.jdt.debug.ui.Messages;
import org.jboss.tools.common.jdt.debug.ui.RemoteDebugUIActivator;
import org.osgi.service.prefs.BackingStoreException;

/**
 * 
 * @author snjeza
 *
 */
public class RemoteDebugPreferencePage extends PreferencePage implements
		IWorkbenchPreferencePage {

	private static final String ADD_ALL = " Add All>> ";
	private static final String ADD = " Add>> ";
	private static final String REMOVE_ALL = " <<Remove All ";
	private static final String REMOVE = " <Remove ";
	private Composite parentComposite;
	private Button autoConnectButton;
	private Button removeButton;
	private Button removeAllButton;
	private Button addButton;
	private Button addAllButton;
	private ListViewer eclipseConfigurationsViewer;
	private ListViewer jbossConfigurationsViewer;
	private Set<ILaunchConfiguration> jbossConfigurations = new HashSet<ILaunchConfiguration>();
	private Set<ILaunchConfiguration> eclipseConfigurations = new HashSet<ILaunchConfiguration>();
	private Set<ILaunchConfiguration> selectedEclipseConfigurations = new HashSet<ILaunchConfiguration>();
	private Set<ILaunchConfiguration> selectedJBossConfigurations = new HashSet<ILaunchConfiguration>();
	
	private ILaunchConfigurationListener launchConfigurationListener = new ILaunchConfigurationListener() {
		
		private void updateLaunchConfiguration(ILaunchConfiguration configuration) {
			try {
				if (!RemoteDebugActivator.REMOTE_JAVA_APPLICATION_ID
						.equals(configuration.getType().getIdentifier())) {
					return;
				}
				refreshConfigurations();
				eclipseConfigurationsViewer.setInput(eclipseConfigurations.toArray(new ILaunchConfiguration[0]));
				jbossConfigurationsViewer.setInput(jbossConfigurations.toArray(new ILaunchConfiguration[0]));
			} catch (CoreException e) {
				RemoteDebugUIActivator.log(e);
			}
		}
		@Override
		public void launchConfigurationRemoved(ILaunchConfiguration configuration) {
			updateLaunchConfiguration(configuration);
		}
		
		@Override
		public void launchConfigurationChanged(ILaunchConfiguration configuration) {
			updateLaunchConfiguration(configuration);
		}
		
		@Override
		public void launchConfigurationAdded(ILaunchConfiguration configuration) {
			updateLaunchConfiguration(configuration);
		}
	}; 
	
	@Override
	public void init(IWorkbench workbench) {
		DebugPlugin.getDefault().getLaunchManager().addLaunchConfigurationListener(launchConfigurationListener);
	}

	protected void refreshConfigurations() {
		ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
		ILaunchConfigurationType type = manager.getLaunchConfigurationType(RemoteDebugActivator.REMOTE_JAVA_APPLICATION_ID);
		jbossConfigurations.clear();
		eclipseConfigurations.clear();
		try {
			ILaunchConfiguration[] configs = manager.getLaunchConfigurations(type);
			for (ILaunchConfiguration config:configs) {
				if (config.getAttribute(RemoteDebugActivator.JBOSS_REMOTE_JAVA_APPLICATION, false)) {
					jbossConfigurations.add(config);
				} else {
					eclipseConfigurations.add(config);
				}
			}
		} catch (CoreException e) {
			RemoteDebugUIActivator.log(e);
		}
		eclipseConfigurationsViewer.setInput(eclipseConfigurations.toArray(new ILaunchConfiguration[0]));
		jbossConfigurationsViewer.setInput(jbossConfigurations.toArray(new ILaunchConfiguration[0]));
        selectedJBossConfigurations.clear();
        selectedEclipseConfigurations.clear();
        eclipseConfigurationsViewer.setSelection(new StructuredSelection(selectedEclipseConfigurations));
        jbossConfigurationsViewer.setSelection(new StructuredSelection(selectedJBossConfigurations));
		configureButtons();
	}

	@Override
	protected Control createContents(Composite parent) {
		initializeDialogUnits(parent);
		parentComposite = parent;
		
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout(1, false);
		composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		composite.setLayout(layout);
		createContentsTools(composite);
		
		autoConnectButton = new Button(composite, SWT.CHECK);
		autoConnectButton.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
		autoConnectButton.setSelection(RemoteDebugUIActivator.getDefault().isAutoConnect());
		autoConnectButton.setText("Automatically connect if only one application found");
		
		Group remoteConfigurationsGroup = new Group(composite, SWT.NONE);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, false);
        layout = new GridLayout(3, false);
        remoteConfigurationsGroup.setLayout(layout);
        remoteConfigurationsGroup.setLayoutData(gd);
        remoteConfigurationsGroup.setText("Remote Configurations");
        
        Composite eclipseConfigurationsComposite = new Composite(remoteConfigurationsGroup, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.FILL, true, false);
        eclipseConfigurationsComposite.setLayoutData(gd);
        eclipseConfigurationsComposite.setLayout(new GridLayout(1, false));
        
        Label eclipseConfigurationLabel = new Label(eclipseConfigurationsComposite, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.FILL, true, false);
        eclipseConfigurationLabel.setLayoutData(gd);
        eclipseConfigurationLabel.setText("Eclipse Remote Java Configurations:");
        
        eclipseConfigurationsViewer = new ListViewer(eclipseConfigurationsComposite, SWT.BORDER | SWT.MULTI);
        gd = new GridData(SWT.FILL, SWT.FILL, true, false);
        gd.heightHint = 300;
        
        eclipseConfigurationsViewer.getList().setLayoutData(gd);
        eclipseConfigurationsViewer.setContentProvider(new ArrayContentProvider());
        eclipseConfigurationsViewer.setLabelProvider(new RemoteConfigurationsLabelProvider());
        
        Composite buttonsComposite = new Composite(remoteConfigurationsGroup, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.FILL, true, false);
        buttonsComposite.setLayoutData(gd);
        buttonsComposite.setLayout(new GridLayout(1, false));
        
        Label buttonsLabel = new Label(buttonsComposite, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.FILL, true, false);
        buttonsLabel.setLayoutData(gd);
        
        Composite buttonsComp = new Composite(buttonsComposite, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.FILL, false, true);
        buttonsComp.setLayoutData(gd);
        buttonsComp.setLayout(new GridLayout());
        
        GC gc = new GC(buttonsComp);
        int maxAddRemoveButtonsWidth = computeMaxAddRemoveButtonsWidth(gc);
        gc.dispose();
        
        removeButton = createButton(buttonsComp, maxAddRemoveButtonsWidth, REMOVE);
        removeAllButton = createButton(buttonsComp, maxAddRemoveButtonsWidth, REMOVE_ALL);
        addButton = createButton(buttonsComp, maxAddRemoveButtonsWidth, ADD);
        addAllButton = createButton(buttonsComp, maxAddRemoveButtonsWidth, ADD_ALL);
        
        Composite jbossConfigurationsComposite = new Composite(remoteConfigurationsGroup, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.FILL, true, false);
        jbossConfigurationsComposite.setLayoutData(gd);
        jbossConfigurationsComposite.setLayout(new GridLayout());
        
        Label jbossConfigurationLabel = new Label(jbossConfigurationsComposite, SWT.NONE);
        gd = new GridData(SWT.FILL, SWT.FILL, true, false);
        jbossConfigurationLabel.setLayoutData(gd);
        jbossConfigurationLabel.setText("JBoss Remote Java Configurations:");
        
        jbossConfigurationsViewer = new ListViewer(jbossConfigurationsComposite, SWT.BORDER | SWT.MULTI);
        gd = new GridData(SWT.FILL, SWT.FILL, true, false);
        gd.heightHint = 300;
        jbossConfigurationsViewer.getList().setLayoutData(gd);
        jbossConfigurationsViewer.setContentProvider(new ArrayContentProvider());
        jbossConfigurationsViewer.setLabelProvider(new RemoteConfigurationsLabelProvider());
        
        Button remoteConfigurationButton = new Button(composite, SWT.PUSH);
		remoteConfigurationButton.setLayoutData(new GridData(SWT.BEGINNING, SWT.FILL, true, false));
		remoteConfigurationButton.setText("Configure Remote Java Application...");
		remoteConfigurationButton.setImage(DebugPluginImages
				.getImage(IDebugUIConstants.IMG_ACT_DEBUG));
		remoteConfigurationButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				DebugPlugin.getDefault().getLaunchManager().removeLaunchConfigurationListener(launchConfigurationListener);
				LaunchConfigurationManager lcManager = DebugUIPlugin.getDefault().getLaunchConfigurationManager();
				LaunchGroupExtension group = lcManager.getLaunchGroup(RemoteDebugActivator.LAUNCH_CATEGORY);
				LaunchConfigurationsDialog dialog = new LaunchConfigurationsDialog(getShell(), group);
				//ILaunchManager manager = DebugPlugin.getDefault().getLaunchManager();
				ILaunchConfiguration config = null;
				if (selectedJBossConfigurations.size() > 0) {
					config = selectedJBossConfigurations.iterator().next();
				} else if (selectedEclipseConfigurations.size() > 0) {
					config = selectedEclipseConfigurations.iterator().next();
				} else if (jbossConfigurations.size() > 0) {
					config = jbossConfigurations.iterator().next();
				} else if (eclipseConfigurations.size() > 0) {
					config = eclipseConfigurations.iterator().next();
				}
				if (config != null) {
					IStructuredSelection selection = new StructuredSelection(config);
					dialog.setInitialSelection(selection);
					dialog.setOpenMode(LaunchConfigurationsDialog.LAUNCH_CONFIGURATION_DIALOG_OPEN_ON_SELECTION);
				} else {
					dialog.setOpenMode(LaunchConfigurationsDialog.LAUNCH_CONFIGURATION_DIALOG_OPEN_ON_LAST_LAUNCHED);
				}
				dialog.open();
				DebugPlugin.getDefault().getLaunchManager().addLaunchConfigurationListener(launchConfigurationListener);
				refreshConfigurations();
			}
		
		});
		
        eclipseConfigurationsViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				ISelection sel = event.getSelection();
				selectedEclipseConfigurations.clear();
				if (sel instanceof IStructuredSelection) {
					IStructuredSelection selection = (IStructuredSelection) sel;
					Iterator iterator = selection.iterator();
					while (iterator.hasNext()) {
						Object object = iterator.next();
						if (object instanceof ILaunchConfiguration) {
							selectedEclipseConfigurations.add((ILaunchConfiguration) object);
						}
					}
				}
				configureButtons();
			}
		});
        jbossConfigurationsViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				ISelection sel = event.getSelection();
				selectedJBossConfigurations.clear();
				if (sel instanceof IStructuredSelection) {
					IStructuredSelection selection = (IStructuredSelection) sel;
					Iterator iterator = selection.iterator();
					while (iterator.hasNext()) {
						Object object = iterator.next();
						if (object instanceof ILaunchConfiguration) {
							selectedJBossConfigurations.add((ILaunchConfiguration) object);
						}
					}
				}
				configureButtons();
			}
		});
        
        removeButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				changeConfigurations(selectedJBossConfigurations, false);
			}
		
        });
        removeAllButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				changeConfigurations(jbossConfigurations, false);
			}
		
        });
        addButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				changeConfigurations(selectedEclipseConfigurations, true);
			}
		
        });
        addAllButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				changeConfigurations(eclipseConfigurations, true);
			}
		
        });
        refreshConfigurations();
        
		return composite;
	}

	private void configureButtons() {
		removeButton.setEnabled(selectedJBossConfigurations.size() > 0);
		removeAllButton.setEnabled(jbossConfigurations.size() > 0);
		addButton.setEnabled(selectedEclipseConfigurations.size() > 0);
		addAllButton.setEnabled(eclipseConfigurations.size() > 0);
	}

	protected Button createButton(Composite buttonsComp,
			int maxAddRemoveButtonsWidth, String text) {
		GridData gd;
		Button button = new Button(buttonsComp, SWT.NONE | SWT.LEFT);
        gd = new GridData();
        gd.verticalAlignment = GridData.VERTICAL_ALIGN_CENTER;
        gd.widthHint = maxAddRemoveButtonsWidth;
        button.setLayoutData(gd);
        button.setText(text);
        return button;
	}

	private int computeMaxAddRemoveButtonsWidth(GC gc) {
		int maxWidth = 0;

		maxWidth = getGreaterWidth(gc,REMOVE, maxWidth);
		maxWidth = getGreaterWidth(gc,REMOVE_ALL, maxWidth);
		maxWidth = getGreaterWidth(gc,ADD, maxWidth);
		maxWidth = getGreaterWidth(gc,ADD_ALL, maxWidth);
		
		return maxWidth;
	}
	
	private int getGreaterWidth(GC gc, String str, int compareWidth) {
		int greaterWidth = compareWidth;

		Point strExtentPoint = gc.stringExtent(str);
		int strWidth = strExtentPoint.x;
		if (strWidth > compareWidth) {
			greaterWidth = strWidth;
		}

		return greaterWidth + 5;
	}

	@Override
	public void dispose() {
		DebugPlugin.getDefault().getLaunchManager().removeLaunchConfigurationListener(launchConfigurationListener);
		super.dispose();
	}
	
	@Override
	protected void performApply() {
		IEclipsePreferences preferences = RemoteDebugUIActivator.getDefault().getPreferences();
		preferences.putBoolean(RemoteDebugUIActivator.AUTO_CONNECT, autoConnectButton.getSelection());
		RemoteDebugUIActivator.getDefault().savePreferences();
		performOkTools();
	}

	@Override
	protected void performDefaults() {
		IEclipsePreferences preferences = RemoteDebugUIActivator.getDefault().getPreferences();
				
		autoConnectButton.setSelection(RemoteDebugUIActivator.AUTO_CONNECT_DEFAULT);
		preferences.putBoolean(RemoteDebugUIActivator.AUTO_CONNECT, RemoteDebugUIActivator.AUTO_CONNECT_DEFAULT);
		RemoteDebugUIActivator.getDefault().savePreferences();
		super.performDefaults();
	}

	@Override
	public boolean performOk() {
		performApply();
		return super.performOk();
	}
	
	protected void changeConfigurations(Set<ILaunchConfiguration> configurations, boolean value) {
		DebugPlugin.getDefault().getLaunchManager().removeLaunchConfigurationListener(launchConfigurationListener);
		for (ILaunchConfiguration configuration:configurations) {
			try {
				ILaunchConfigurationWorkingCopy wc = configuration.getWorkingCopy();
				wc.setAttribute(RemoteDebugActivator.JBOSS_REMOTE_JAVA_APPLICATION, value);
				wc.doSave();
			} catch (CoreException e) {
				RemoteDebugUIActivator.log(e);
			}
		}
		DebugPlugin.getDefault().getLaunchManager().addLaunchConfigurationListener(launchConfigurationListener);
		refreshConfigurations();
	}

	class RemoteConfigurationsLabelProvider extends LabelProvider {
		public Image getImage(Object element) {
	        return null;
	      }

	      public String getText(Object element) {
	    	  if (element instanceof ILaunchConfiguration) {
	    		  ILaunchConfiguration configuration = (ILaunchConfiguration) element;
	    		  return configuration.getName();
	    	  }
	        return null;
	      }
	}

	
	
	/*
	 * Code relating to tools.jar and jdk
	 */
	
	private Label descriptionLabel;
	private Combo vmInstallCombo;

    private IVMInstall[] matchingVMs;
    private String[] vmNames;
    
    private IVMInstallChangedListener listener;
    protected Control createContentsTools(Composite parent) {
        Group composite = new Group(parent, SWT.NONE);
        composite.setText("Tools.jar");
        GridLayout layout = new GridLayout(1, false);
        layout.marginHeight = 5;
        layout.marginWidth = 5;
        composite.setLayout(layout);

        descriptionLabel = new Label(composite, SWT.WRAP);
        descriptionLabel.setText(org.jboss.tools.common.jdt.debug.ui.Messages.PreferencePageToolsJarDesc);
        
        updateDescriptionLabel();
        
        GridData layoutData = new GridData(GridData.FILL_HORIZONTAL);
        layoutData.widthHint = 500;
        descriptionLabel.setLayoutData(layoutData);
        createJdkRootDirectoryGroup(composite);
        
        applyDialogFont(composite);

        listener = new IVMInstallChangedListener() {
			public void defaultVMInstallChanged(IVMInstall previous,
					IVMInstall current) {
			}
			public void vmChanged(PropertyChangeEvent event) {
			}
			public void vmAdded(IVMInstall vm) {
				resetCombo();
			}
			public void vmRemoved(IVMInstall vm) {
				resetCombo();
			}
        };
        JavaRuntime.addVMInstallChangedListener(listener);
        
        return composite;
    }

    public boolean performOkTools() {
    	int ind = vmInstallCombo.getSelectionIndex();
    	IEclipsePreferences prefs = InstanceScope.INSTANCE.getNode(RemoteDebugActivator.PLUGIN_ID);
    	prefs.put(IPropertyKeys.JDK_VM_INSTALL, matchingVMs[ind].getId());
    	try {
    		prefs.flush();
    	} catch(BackingStoreException bse) {
    		
    	}
        updateDescriptionLabel();
        return true;
    }

    protected void updateDescriptionLabel() {
        boolean isReady = ToolsCore.isToolsReady();
        File toolsJar = ToolsCore.getToolsJar();
    	if( isReady ) {
	        if( toolsJar == null ) {
        		descriptionLabel.setText(Messages.PreferencePageToolsReadyNoJar);
	        } else {
        		descriptionLabel.setText(NLS.bind(Messages.PreferencePageToolsReadyWithJar, toolsJar.getAbsolutePath()));
	        }
    	} else {
	        if( toolsJar == null ) {
        		descriptionLabel.setText(Messages.PreferencePageToolsNotReadyNotFound);
	        } else {
        		descriptionLabel.setText(NLS.bind(Messages.PreferencePageToolsNotReadyLoadFailed, toolsJar.getAbsolutePath()));
	        }
    	}
    	parentComposite.layout();
    }
    
    /**
     * Creates the JDK root directory group.
     *
     * @param parent
     *            The parent composite
     */
    private void createJdkRootDirectoryGroup(Composite parent) {
        Composite composite = new Composite(parent, SWT.NONE);
        composite.setLayout(new GridLayout(3, false));
        composite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        Label label = new Label(composite, SWT.NONE);
        label.setText("JDK: ");

        vmInstallCombo = new Combo(composite, SWT.BORDER | SWT.READ_ONLY);
        resetCombo();
        
        vmInstallCombo.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        vmInstallCombo.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent e) {
                validateJdkRootDirectory();
            }
        });

        Button button = new Button(composite, SWT.NONE);
        button.setText("Installed JREs...");
        button.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
            	PreferencesUtil.createPreferenceDialogOn(getShell(),"org.eclipse.jdt.debug.ui.preferences.VMPreferencePage", null, null);
            }
        });
    }
    
    private void resetCombo() {
        matchingVMs = getMatchingVMInstalls();
        vmNames = getVMNames(matchingVMs);
        vmInstallCombo.setItems(vmNames);
        
        int selInd = -1;
        IVMInstall backup = ToolsCore.getJdkVMInstall();
        if( backup != null ) {
        	selInd = Arrays.asList(matchingVMs).indexOf(backup);
        }
        if( selInd != -1) {
        	vmInstallCombo.select(selInd);
        }
    }

    
    private String[] getVMNames(IVMInstall[] vms) {
    	String[] ret = new String[vms.length];
    	for( int i = 0; i < vms.length; i++ ) {
    		ret[i] = vms[i].getName();
    	}
    	return ret;
    }

    private static IVMInstall[] getMatchingVMInstalls() {
    	return ToolsCore.getAllCompatibleInstalls();
    }


    /**
     * Validates the JDK root directory.
     */
    void validateJdkRootDirectory() {
    	int selInd = vmInstallCombo.getSelectionIndex();
    	String jdkRootDirectory = null;
    	if( selInd != -1 ) {
    		jdkRootDirectory = matchingVMs[selInd].getInstallLocation().getAbsolutePath();
    	}
        if (jdkRootDirectory == null || jdkRootDirectory.isEmpty()) {
            setMessage(org.jboss.tools.common.jdt.debug.Messages.jdkRootDirectoryNotEnteredMsg,
                    IMessageProvider.WARNING);
            return;
        }
        String message = ToolsCore.validateJdkRootDirectory(jdkRootDirectory);
        setMessage(message, IMessageProvider.WARNING);
    }

}
