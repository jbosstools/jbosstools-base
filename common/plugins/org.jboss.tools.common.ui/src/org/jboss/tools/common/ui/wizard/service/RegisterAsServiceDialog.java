/******************************************************************************* 
 * Copyright (c) 2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.ui.wizard.service;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.Flags;
import org.eclipse.jdt.core.IType;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.jboss.tools.common.java.IParametedType;
import org.jboss.tools.common.java.ParametedType;
import org.jboss.tools.common.java.ParametedTypeFactory;
import org.jboss.tools.common.ui.CommonUIMessages;
import org.jboss.tools.common.ui.CommonUIPlugin;
import org.jboss.tools.common.ui.CommonUIImages;
import org.jboss.tools.common.ui.widget.editor.IFieldEditor;
import org.jboss.tools.common.ui.widget.editor.IFieldEditorFactory;

public class RegisterAsServiceDialog extends TitleAreaDialog {
	IType type;
	Map<String, IParametedType> types;
	IFieldEditor serviceTypeSelector;
	String result;

	public RegisterAsServiceDialog(Shell parentShell, IType type) {
		super(parentShell);
		this.type = type;
		initTypes();

		setShellStyle(getShellStyle() | SWT.RESIZE);
		List<String> serviceTypeNames = new ArrayList<String>(types.keySet());
		String defaultValue = serviceTypeNames.isEmpty() ? "" : serviceTypeNames.get(0);
		serviceTypeSelector = IFieldEditorFactory.INSTANCE.createComboEditor("serviceType", //$NON-NLS-1$
				CommonUIMessages.REGISTER_AS_SERVICE_TYPE_LABEL, serviceTypeNames, defaultValue);
	}

	void initTypes() {
    	ParametedType parametedType = new ParametedTypeFactory().newParametedType(type);
    	Collection<IParametedType> ts = parametedType.getAllTypes();
    	Map<String, IParametedType> types = new TreeMap<String, IParametedType>();
    	for (IParametedType t: ts) {
    		if(t.getType() != null) {
    			String q = t.getType().getFullyQualifiedName();
    			types.put(q, t);
    		}
    	}
    	types.remove("java.lang.Object"); //$NON-NLS-1$
    	types.remove(type.getFullyQualifiedName());
    	this.types = types;
	}

    private final int DIALOG_WIDTH = 400;
	private final int DIALOG_HEIGHT = 60;

	protected Control createDialogArea(Composite parent) {
		getShell().setText(CommonUIMessages.REGISTER_AS_SERVICE_TITLE);
		setTitle(NLS.bind(CommonUIMessages.REGISTER_AS_SERVICE_SUB_TITLE, type.getFullyQualifiedName()));
		setTitleImage(CommonUIImages.getInstance().getOrCreateImage(CommonUIImages.JAVA_SERVICE_PROVIDER_IMAGE)); // image is managed by registry
		setMessage(CommonUIMessages.REGISTER_AS_SERVICE_MESSAGE);
		if(types.isEmpty()) {
			setErrorMessage(CommonUIMessages.REGISTER_AS_SERVICE_NO_TYPES_MESSAGE);
		}
		
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout gridLayout = new GridLayout(1, false);
		gridLayout.marginHeight = 5;
		gridLayout.marginWidth = 5;
		gridLayout.horizontalSpacing = 10;
		gridLayout.verticalSpacing = 0;
		composite.setLayout(gridLayout);
		
		Label dialogAreaSeparator = new Label(composite, SWT.HORIZONTAL | SWT.SEPARATOR);
		dialogAreaSeparator.setLayoutData(new GridData(SWT.FILL, SWT.NONE, true, false));
		
		Control pageArea = createField(composite);
		GridData gd = new GridData(SWT.FILL, SWT.BEGINNING, true, true);
		pageArea.setLayoutData(gd);
		
		dialogAreaSeparator = new Label(composite, SWT.HORIZONTAL | SWT.SEPARATOR);
		dialogAreaSeparator.setLayoutData(new GridData(SWT.FILL, SWT.NONE, true, false));
		
		gd = new GridData(SWT.FILL, SWT.FILL, true, true);
		gd.widthHint = DIALOG_WIDTH;
		gd.heightHint = DIALOG_HEIGHT;
		composite.setLayoutData(gd);
		
		return composite;
	}

	Control createField(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout gridLayout = new GridLayout(2, false);
		gridLayout.marginHeight = 10;
		gridLayout.marginWidth = 0;
		gridLayout.horizontalSpacing = 10;
		gridLayout.verticalSpacing = 10;
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		composite.setLayoutData(gd);
		composite.setLayout(gridLayout);
		serviceTypeSelector.doFillIntoGrid(composite);
		Object[] os = serviceTypeSelector.getEditorControls();
		if(os.length == 2 && os[1] instanceof Combo) {
			((Combo)os[1]).setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		}
		serviceTypeSelector.addPropertyChangeListener(new PropertyChangeListener() {
			
			@Override
			public void propertyChange(PropertyChangeEvent evt) {
				validate();
			}
		});
		return composite;
	}

	void validate() {
		String serviceType = serviceTypeSelector.getValueAsString();
		if(serviceType == null || serviceType.length() == 0) {
			return;
		}
		IParametedType type = types.get(serviceType);
		String warning = null;
		if(type != null) {
			IType t = type.getType();
			try {
				if(!t.isInterface() && !Flags.isAbstract(t.getFlags())) {
					warning = CommonUIMessages.REGISTER_AS_SERVICE_NON_ABSTRACT_MESSAGE;
				}
			} catch (CoreException e) {
				CommonUIPlugin.getDefault().logError(e);
			}
		}
		String typeName = this.type.getFullyQualifiedName();
		if(RegisterServiceUtil.isServiceRegistered(type.getType().getJavaProject().getProject(), typeName, serviceType)) {
			getButton(IDialogConstants.OK_ID).setEnabled(false);
			setErrorMessage(CommonUIMessages.REGISTER_AS_SERVICE_ALREADY_REGISTERED_MESSAGE);
		} else {
			getButton(IDialogConstants.OK_ID).setEnabled(true);
			setErrorMessage(null);
			if(warning != null) {
				setMessage(warning, IMessageProvider.WARNING);
			} else {
				setMessage(CommonUIMessages.REGISTER_AS_SERVICE_MESSAGE);
			}
		}
	}

	protected Control createContents(Composite parent) {
		Control c = super.createContents(parent);
		if(types.isEmpty()) {
			Button ok = getButton(IDialogConstants.OK_ID);
			ok.setEnabled(false);
		}
		validate();
		return c;
	}

	public void okPressed() {
		result = serviceTypeSelector.getValueAsString();
		super.okPressed();
	}

	public String getResult() {
		return result;
	}

	public void setServiceType(String type) {
		if(!types.containsKey(type)) {
			throw new IllegalArgumentException(type);
		}
		serviceTypeSelector.setValue(type);
	}
}
