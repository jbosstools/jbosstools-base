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
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jdt.core.Flags;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.internal.ui.IJavaHelpContextIds;
import org.eclipse.jdt.internal.ui.dialogs.OpenTypeSelectionDialog;
import org.eclipse.jdt.internal.ui.dialogs.StatusInfo;
import org.eclipse.jdt.internal.ui.dialogs.StatusUtil;
import org.eclipse.jdt.internal.ui.wizards.dialogfields.SelectionButtonDialogFieldGroup;
import org.eclipse.jdt.ui.wizards.NewClassWizardPage;
import org.eclipse.jdt.ui.wizards.NewTypeWizardPage;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.common.ui.CommonUIMessages;
import org.jboss.tools.common.ui.CommonUIPlugin;
import org.jboss.tools.common.ui.IValidator;
import org.jboss.tools.common.ui.CommonUIImages;
import org.jboss.tools.common.ui.widget.editor.ButtonFieldEditor.ButtonPressedAction;
import org.jboss.tools.common.ui.widget.editor.IFieldEditor;
import org.jboss.tools.common.ui.widget.editor.SwtFieldEditorFactory;
import org.jboss.tools.common.util.EclipseJavaUtil;

/**
 * 
 * @author Viacheslav Kabanovich
 * 
 */
public class NewServiceWizardPage extends NewClassWizardPage {
	String defaultTypeName = null;

	protected StatusInfo interfaceNameStatus = new StatusInfo();

	IFieldEditor interfaceField;

	public NewServiceWizardPage() {
		setTitle(CommonUIMessages.NEW_SERVICE_WIZARD_PAGE_NAME);
		setDescription(CommonUIMessages.NEW_SERVICE_WIZARD_DESCRIPTION);
		setImageDescriptor(CommonUIImages.getInstance().getOrCreateImageDescriptor(CommonUIImages.NEW_JAVA_SERVICE_PROVIDER_IMAGE));
	}

	public void init(IStructuredSelection selection) {
		super.init(selection);
		defaultTypeName = null;
		if (selection != null && !selection.isEmpty()) {
			Object o = selection.iterator().next();
			IType type = null;
			if (o instanceof IType) {
				type = (IType) o;
			} else if (o instanceof ICompilationUnit) {
				ICompilationUnit cu = (ICompilationUnit) o;
				try {
					IType[] ts = cu.getTypes();
					if (ts != null && ts.length > 0)
						type = ts[0];
				} catch (JavaModelException e) {
					CommonUIPlugin.getDefault().logError(e);
				}

			}
			boolean isInterface = false;
			try {
				isInterface = type != null && type.isInterface();
			} catch (JavaModelException e) {
				CommonUIPlugin.getDefault().logError(e);
			}
			if (isInterface) {
				ArrayList<String> interfacesNames = new ArrayList<String>();
				String name = "";
				try {
					name = type.getFullyQualifiedParameterizedName();
				} catch (JavaModelException e) {
					name = type.getFullyQualifiedName();
				}
				interfacesNames.add(name);
				setSuperInterfaces(interfacesNames, true);
				superInterfacesChanged();
				setDefaultTypeName(name);
			}
		}
		setModifiers(getModifiers(), true);
		try {
			Field f = NewTypeWizardPage.class.getDeclaredField("fOtherMdfButtons");
			if(f != null) {
				f.setAccessible(true);
				SelectionButtonDialogFieldGroup g = (SelectionButtonDialogFieldGroup)f.get(this);
				g.enableSelectionButton(0, false);
			}
		} catch (Exception e) {
			CommonUIPlugin.getDefault().logError(e);
		}
		doStatusUpdate();
	}

	void setDefaultTypeName(String interfaceName) {
		int d = interfaceName.lastIndexOf('.');
		int b = interfaceName.indexOf('<');
		if (b < 0)
			b = interfaceName.length();
		String elementName = interfaceName.substring(d + 1, b);
		String typeName = elementName + "Service";
//		typeName += interfaceName.substring(b);

		String currentTypeName = getTypeName();
		boolean isDefault = currentTypeName == null || currentTypeName.length() == 0 || currentTypeName.equals(defaultTypeName);
		if(isDefault)  {
			setTypeName(typeName, true);
			typeNameChanged();
		}
		defaultTypeName = typeName;
	}

	public void createControl(Composite parent) {
		initializeDialogUnits(parent);

		Composite composite = new Composite(parent, SWT.NONE);
		composite.setFont(parent.getFont());

		int nColumns = 4;

		GridLayout layout = new GridLayout();
		layout.numColumns = nColumns;
		composite.setLayout(layout);

		// pick & choose the wanted UI components

		createContainerControls(composite, nColumns);
		createPackageControls(composite, nColumns);
		// createEnclosingTypeControls(composite, nColumns);

		createSeparator(composite, nColumns);

		createTypeNameControls(composite, nColumns);
		createModifierControls(composite, nColumns);

		// createSuperClassControls(composite, nColumns);
		// createSuperInterfacesControls(composite, nColumns);

		// createMethodStubSelectionControls(composite, nColumns);

		createCustomFields(composite);

		createCommentControls(composite, nColumns);
		enableCommentControl(true);

		setControl(composite);

		Dialog.applyDialogFont(composite);
		PlatformUI.getWorkbench().getHelpSystem()
				.setHelp(composite, IJavaHelpContextIds.NEW_CLASS_WIZARD_PAGE);

		onInterfaceChange();
		doStatusUpdate();
	}

	protected void createTypeMembers(IType newType,
			final ImportsManager imports, IProgressMonitor monitor)
			throws CoreException {
		createInheritedMethods(newType, true, true, imports,
				new SubProgressMonitor(monitor, 1));
	}

	protected void createCustomFields(Composite composite) {
		createInterfaceField(composite);
	}

	protected void createInterfaceField(Composite composite) {
		interfaceField = SwtFieldEditorFactory.INSTANCE.createButtonFieldEditor("Interface", 
			CommonUIMessages.NEW_SERVICE_WIZARD_INTERFACES_LABEL, "", 
			new ButtonPressedAction(CommonUIMessages.SWT_FIELD_EDITOR_FACTORY_BROWS) {
				public void run() {
					OpenTypeSelectionDialog dialog = new OpenTypeSelectionDialog(getShell(), false, getContainer(),
							getJavaProject() != null
							? SearchEngine.createJavaSearchScope(new IJavaProject[] {getJavaProject()})
							: SearchEngine.createJavaSearchScope(new IJavaProject[0]), 
							IJavaSearchConstants.CLASS_AND_INTERFACE);
					dialog.setTitle(CommonUIMessages.NEW_SERVICE_WIZARD_SELECT_SERVICE_TYPE_TITLE);
					if(dialog.open() == IDialogConstants.OK_ID) {
						Object[] os = dialog.getResult();
						if(os != null && os.length == 1) {
							IType t = (IType)os[0];
							interfaceField.setValue(t.getFullyQualifiedName());
						}
					}
				}
		    }, 
			new IValidator() {
				@Override
				public Map<String, IStatus> validate(Object value, Object context) {
					return null;
				}
			});
		List<String> s = getSuperInterfaces();
		if(!s.isEmpty()) {
			setServiceType(s.get(0));
		}
		interfaceField.doFillIntoGrid(composite);
		interfaceField.addPropertyChangeListener(new PropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent evt) {
				onInterfaceChange();
			}
		});
	}

	public void setServiceType(String typeName) {
		interfaceField.setValue(typeName);
	}

	protected void onInterfaceChange() {
		interfaceNameStatus = new StatusInfo();
		String q = interfaceField == null ? "" : interfaceField.getValue().toString().trim();
		
		boolean isInterface = false, isClass = false;

		if(getJavaProject() != null && q.length() > 0) {
			String qRaw = q;
			if(qRaw.indexOf('<') > 0) {
				qRaw = qRaw.substring(0, qRaw.indexOf('<'));
			}
			try {
				IType t = EclipseJavaUtil.findType(getJavaProject(), qRaw);
				if(t != null) {
					isInterface = t.isInterface();
					isClass = !t.isInterface();
					if(Flags.isFinal(t.getFlags())) {
						interfaceNameStatus.setError(CommonUIMessages.NEW_SERVICE_WIZARD_SERVICE_TYPE_FINAL);
					} else if(isClass && !Flags.isAbstract(t.getFlags())) {
						interfaceNameStatus.setWarning(CommonUIMessages.NEW_SERVICE_WIZARD_SERVICE_TYPE_CONCRETE);
					}
				} else {
					isInterface = true;
					interfaceNameStatus.setError(NLS.bind(CommonUIMessages.NEW_SERVICE_WIZARD_SERVICE_TYPE_NOT_EXISTS, q));
				}
			} catch (JavaModelException e) {
				CommonUIPlugin.getDefault().logError(e);
				interfaceNameStatus.setError(NLS.bind(CommonUIMessages.NEW_SERVICE_WIZARD_SERVICE_TYPE_NOT_EXISTS, q));
			}
		}
		if(q.length() == 0) {
			interfaceNameStatus.setError(CommonUIMessages.NEW_SERVICE_WIZARD_SERVICE_TYPE_EMPTY);
		}
		if(isInterface) {
			List<String> is = new ArrayList<String>();
			if(q.length() > 0) is.add(q.trim());
			setSuperInterfaces(is, true);
			setSuperClass("", true);
		} else if(isClass) {
			setSuperInterfaces(new ArrayList<String>(), true);
			setSuperClass(q, true);
		}
		
		doStatusUpdate();
	}

	@Override
	protected IStatus superInterfacesChanged() {
		List list = getSuperInterfaces();
		if(list != null && !list.isEmpty()) {
			setDefaultTypeName(list.get(0).toString());
		}
		StatusInfo result = (StatusInfo) super.superInterfacesChanged();
		return result;
	}

	@Override
	protected IStatus superClassChanged() {
		String n = getSuperClass();
		if(n != null && n.length() > 0 && !n.endsWith("Object")) {
			setDefaultTypeName(n);
		}
		StatusInfo result = (StatusInfo) super.superClassChanged();
		return result;
	}

	protected void updateStatus(IStatus[] status) {
		if(!CommonUIMessages.NEW_SERVICE_WIZARD_DESCRIPTION.equals(getDescription())) {
			setDescription(CommonUIMessages.NEW_SERVICE_WIZARD_DESCRIPTION);
		}
		IStatus[] ns = new IStatus[status.length + 1];
		System.arraycopy(status, 0, ns, 0, status.length);
		ns[status.length] = interfaceNameStatus;
		status = ns;
		updateStatus(StatusUtil.getMostSevere(status));
	}
	
	public boolean isToBeRegisteredInMetaInf() {
		return true;
	}

	@Override
	public void setVisible(boolean visible) {
		if(!getControl().isVisible() && visible && fSuperInterfacesStatus.matches(IStatus.ERROR) && !fTypeNameStatus.matches(IStatus.ERROR)) {
			setDescription(fSuperInterfacesStatus.getMessage());
		}
		super.setVisible(visible);
	}

	protected String getSuperInterfacesLabel() {
		return CommonUIMessages.NEW_SERVICE_WIZARD_INTERFACES_LABEL;
	}

	public String getServiceRawType() {
		return (interfaceField == null) ? "" : toRawTypeName(interfaceField.getValueAsString());
	}

	private static String toRawTypeName(String typeName) {
		return typeName.indexOf('<') >= 0 ? typeName.substring(0, typeName.indexOf('<')) : typeName;
	}
}
