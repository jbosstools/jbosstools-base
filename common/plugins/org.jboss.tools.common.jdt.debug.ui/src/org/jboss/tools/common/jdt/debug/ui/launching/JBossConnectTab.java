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
package org.jboss.tools.common.jdt.debug.ui.launching;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.jface.dialogs.Dialog;
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
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.jboss.tools.common.jdt.debug.RemoteDebugActivator;
import org.jboss.tools.common.jdt.debug.ui.Messages;
import org.jboss.tools.common.jdt.debug.ui.RemoteDebugUIActivator;

/**
 * 
 * @author snjeza
 *
 */
public class JBossConnectTab extends AbstractLaunchConfigurationTab {
	
	private Group messageGroup;
	private Button jbossConfigurationButton;
	private Image image;

	public JBossConnectTab() {
		super();
		image = RemoteDebugUIActivator.imageDescriptorFromPlugin(RemoteDebugUIActivator.PLUGIN_ID, "/icons/jboss.gif").createImage();
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
		
		createVerticalSpacer(comp, 2);
		jbossConfigurationButton = createCheckButton(comp, "JBoss Remote Configuration"); 
		jbossConfigurationButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				setDirty(true);
				updateLaunchConfigurationDialog();
			}
		
		});
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
				updateLaunchConfigurationDialog();
				messageGroup.setVisible(!RemoteDebugActivator.getDefault().isJdk());
			}
		});
		
		setControl(comp);
	}
	
	private void updateMarkAsJBossFromConfig(ILaunchConfiguration config) {
		boolean markAsJBoss = false;
		try {
			markAsJBoss = config.getAttribute(RemoteDebugActivator.JBOSS_REMOTE_JAVA_APPLICATION, false);	
		}
		catch (CoreException ce) {RemoteDebugUIActivator.log(ce);}
		jbossConfigurationButton.setSelection(markAsJBoss);	
	}

	public void performApply(ILaunchConfigurationWorkingCopy config) {
		config.setAttribute(RemoteDebugActivator.JBOSS_REMOTE_JAVA_APPLICATION, jbossConfigurationButton.getSelection());
	}

	/* (non-Javadoc)
	 * @see org.eclipse.debug.ui.ILaunchConfigurationTab#setDefaults(org.eclipse.debug.core.ILaunchConfigurationWorkingCopy)
	 */
	public void setDefaults(ILaunchConfigurationWorkingCopy config) {
		config.setAttribute(RemoteDebugActivator.JBOSS_REMOTE_JAVA_APPLICATION, false);
	}

	 /* (non-Javadoc)
	 * @see org.eclipse.debug.ui.AbstractLaunchConfigurationTab#isValid(org.eclipse.debug.core.ILaunchConfiguration)
	 */
	public boolean isValid(ILaunchConfiguration config) {	
		setErrorMessage(null);
		setMessage(null);	
		return true;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.debug.ui.ILaunchConfigurationTab#getName()
	 */
	public String getName() {
		return "JBoss Preferences";
	}			
	
	/**
	 * @see org.eclipse.debug.ui.AbstractLaunchConfigurationTab#getId()
	 * 
	 * @since 3.3
	 */
	public String getId() {
		return "org.jboss.tools.common.jdt.debug.ui.launching.launchConfigurationTabGroup.JBossRemoteJavaApplication2"; //$NON-NLS-1$
	}

	@Override
	public void initializeFrom(ILaunchConfiguration configuration) {
		updateMarkAsJBossFromConfig(configuration);
	}

	@Override
	public Image getImage() {
		return image;
	}

	@Override
	public void dispose() {
		image.dispose();
		super.dispose();
	}

}
