/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.attribute.editor;

import java.util.ArrayList;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IBuffer;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.core.search.IJavaSearchScope;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.internal.ui.dialogs.FilteredTypesSelectionDialog;
import org.eclipse.jdt.internal.ui.dialogs.OpenTypeSelectionDialog;
import org.eclipse.jdt.internal.ui.refactoring.contentassist.JavaTypeCompletionProcessor;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.FilteredItemsSelectionDialog;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.key.WizardKeys;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.jboss.tools.common.model.ui.attribute.adapter.IModelPropertyEditorAdapter;
import org.jboss.tools.common.model.ui.templates.ControlContentAssistHelper;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;
import org.jboss.tools.common.model.ui.wizards.NewClassWizard;
import org.jboss.tools.common.model.ui.wizards.NewTypeWizardAdapter;
import org.jboss.tools.common.model.util.AccessibleJava;
import org.jboss.tools.common.model.util.EclipseJavaUtil;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.util.ModelFeatureFactory;
import org.jboss.tools.common.model.util.XModelObjectUtil;

public class JavaHyperlinkLineFieldEditor extends StringButtonFieldEditorEx
			implements JavaHyperlinkCueLabelProvider.JavaClassHolder {
//	private final static String CANNOT_OPEN_SOURCE_JAVA_PROJECT_FAILED = "JavaHyperlinkLineFieldEditor.CannotOpenJavaSource";
//	private final static String CANNOT_FIND_SOURCE = "JavaHyperlinkLineFieldEditor.CannotFindSource";
	private IProject project;
	private String publicId;
	private String xPath;
	
	AccessibleJava.IContextPackageProvider cpp = null;
	
	public JavaHyperlinkLineFieldEditor() {
		super();
	}

	public JavaHyperlinkLineFieldEditor(IWidgetSettings settings) {
		super(settings);
	}
	
	public void setContextPackageProvider(AccessibleJava.IContextPackageProvider cpp) {
		this.cpp = cpp;
	}
	
	// IPropertyFieldEditor
	public void setPropertyEditor(IPropertyEditor propertyEditor) {
		super.setPropertyEditor(propertyEditor);
		if (propertyEditor!=null) {
			valueProvider = (IValueProvider)propertyEditor.getAdapter(IValueProvider.class);
			// GET Java Project Name!
			if (valueProvider instanceof DefaultValueAdapter) {
				if (((DefaultValueAdapter)valueProvider).getModel()!=null) {
					XModelObject xmo = FileSystemsHelper.getFileSystems(((DefaultValueAdapter)valueProvider).getModel());
					if (xmo!=null) {
						IProject project = (IProject)xmo.getModel().getProperties().get("project"); //$NON-NLS-1$
						if (project!=null) { 
							this.project = project;
						}
					}
					xmo = ((DefaultValueAdapter)valueProvider).getModelObject();
					if (xmo!=null) {
						// get publicId
						publicId = XModelObjectUtil.getDocumentPublicID(xmo);
					}
					XAttribute attr = ((DefaultValueAdapter)valueProvider).getAttribute();
					if (attr!=null) {
						// get xPath
						xPath = XModelObjectUtil.getXMLLocalPath(attr);
						
						AccessibleJava.IContextPackageProvider cpp = getPackageProvider();
						if(cpp != null) {
							setContextPackageProvider(cpp);
						}
						
					}
				}
			}
			valueProvider.addValueChangeListener(this);
			valueChangeListener = (IValueChangeListener)propertyEditor.getAdapter(IValueChangeListener.class);
			if(project != null) {
				setLabelAction(new OpenJavaEditorAction());
				setSelectableLabel(Boolean.TRUE.booleanValue());
				setChangeButtonText(JFaceResources.getString("openBrowse")); //$NON-NLS-1$
			}
		}
	}
	
//	private String getSuperClass(String publicId, String xPath) {
//	    return MetaClassTemplateHelper.instance.getSuperClassName(project, publicId, xPath);
//	}

//	private String[] getInterfaces(String publicId, String xPath) {
//	    return MetaClassTemplateHelper.instance.getInterfacesName(project, publicId, xPath);
//	}
	
	class OpenJavaEditorAction extends Action {
		public void run() {
		    
		    String text = JavaHyperlinkLineFieldEditor.this.getTextField().getText();
		    JavaAdapter javaAdapter = JavaAdapter.getInstance();
		    javaAdapter.init(project, publicId, xPath, text);
		    
		    IJavaElement javaElement = javaAdapter.getJavaElement();
		    
			if (javaElement==null) {
				NewTypeWizardAdapter wizardAdapter = new NewTypeWizardAdapter(project);
				wizardAdapter.setRawClassName(javaAdapter.getClassName());
				wizardAdapter.setRawPackageName(javaAdapter.getPackageName());
				
				String superClassName = javaAdapter.getSuperClass();
				String[] interfacesName = javaAdapter.getInterfacees();

				wizardAdapter.setRawSuperClassName(superClassName);
				if (interfacesName!=null && interfacesName.length>0) {
					for (int i=0;i<interfacesName.length;++i) {
						wizardAdapter.addRawInterfaceName(interfacesName[i]);
					}
				}
				
				NewClassWizard wizard = null;
				
				XAttribute a = ((DefaultValueAdapter)getPropertyEditor().getInput()).getAttribute();
				if(a != null) {
					String cls = a.getProperty("newWizardClass");
					if(cls != null && cls.length() > 0) {
						wizard = (NewClassWizard)ModelFeatureFactory.getInstance().createFeatureInstance(cls);
					}
				}
				if(wizard == null) {
					wizard = new NewClassWizard();
				}
				wizard.setAdapter(wizardAdapter);
				WizardDialog dialog = new WizardDialog(ModelUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell(), wizard);
				dialog.create();
				int result = dialog.open();
				if (result == WizardDialog.OK) {
					String newValue = wizard.getQualifiedClassName();
					JavaHyperlinkLineFieldEditor.this.valueProvider.setValue(newValue);
					if(wizard.getContainer() != null) {
						wizard.performFinish();
					}
				}
				dialog.close();
			} else {
				try {
                    JavaUI.openInEditor(javaElement);
                } catch (CoreException e) {
                	ModelUIPlugin.getPluginLog().logError(e);
                }
			}
			
		}
	}
	
	public IProject getProject() {
		return project;
	}

	public void setProject(IProject project) {
		this.project = project;
	}

	protected void valueChanged() {
		super.valueChanged();
		if (getTextField()!=null) {
			String text = getTextField().getText();
			if ((text!=null)&&(text.length()>0)) {
				if (getLabelAction()!=null) {
					getLabelAction().setEnabled(Boolean.TRUE.booleanValue());
					return;
				}
			}
		}
		if (getLabelAction()!=null) {
			getLabelAction().setEnabled(Boolean.TRUE.booleanValue());
		}
	}

	protected Control createTextChangeControl(Composite parent) {
		Control control = super.createTextChangeControl(parent);
		if (getLabelAction()!=null) {
			getLabelAction().setEnabled(Boolean.TRUE.booleanValue());
		}
		return control;
	}
	
	public Control createTextControl(Composite parent) {
		Control control = super.createTextControl(parent);
        Text text = getTextField();
        text.setData("JavaHyperlinkLineFieldEditor", this); //$NON-NLS-1$

        return control;
	}
	
	boolean classExists;
	String textCache = null;
	long timeStamp = -1;

	public boolean canCreateClass() {
	    String text = (getTextField() != null) ? getTextField().getText()
	    		: (valueProvider != null) ? valueProvider.getStringValue(true) : null;
	    if(text == null) return false;
	    if(text.length() == 0) return true;
	    for (int i = 0; i < text.length(); i++) {
	    	char ch = text.charAt(i);
	    	if(ch != '.' && !Character.isJavaIdentifierPart(ch)) return false;
	    }
	    return true;
	}

	public boolean classExists() {
	    String text = (getTextField() != null) ? getTextField().getText()
	    		: (valueProvider != null) ? valueProvider.getStringValue(true) : null;
	    if(text == null || text.length() == 0) return false;
	    JavaAdapter javaAdapter = JavaAdapter.getInstance();
	    if(text.equals(textCache) && timeStamp == javaAdapter.lastTimeChange) {
	    	return classExists;
	    }
	    javaAdapter.init(project, publicId, xPath, text);

	    timeStamp = javaAdapter.lastTimeChange;
	    textCache = text;
	    
	    return classExists = (javaAdapter.getJavaElement() != null);
	}
	
	protected String getHyperlinkLableToolTip() {
		return classExists() ? "Open" : "Create";
	}

	private AccessibleJava.IContextPackageProvider getPackageProvider() {
		IModelPropertyEditorAdapter adapter = (IModelPropertyEditorAdapter)propertyEditor.getInput();
		XAttribute a = adapter.getAttribute();
		if(a == null) return null;
		String cls = a.getProperty("contextPackageProvider"); //$NON-NLS-1$
		if(cls == null || cls.length() == 0) return null;
		try {
			AccessibleJava.IContextPackageProvider o = (AccessibleJava.IContextPackageProvider)ModelFeatureFactory.getInstance().createFeatureInstance(cls);
			if(o != null) o.setObject(((DefaultValueAdapter)adapter).getModelObject());
			return o;
		} catch (ClassCastException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}		
		return null;
	}

	private IJavaSearchScope getScope(String javaProjectName) {
		IJavaElement[] elements = new IJavaElement[0];
		if(javaProjectName != null) {
			IPath path = new Path(javaProjectName);
			IResource res = ResourcesPlugin.getWorkspace().getRoot().findMember(path);
			IProject proj = res.getProject();
			IJavaProject jproject = JavaCore.create(proj);
			IPackageFragmentRoot fCurrRoot = jproject.getPackageFragmentRoot(res);
			elements = new IJavaElement[] { fCurrRoot.getJavaProject() };
		} else {
			IProject[] ps = ModelPlugin.getWorkspace().getRoot().getProjects();
			ArrayList<IJavaElement> l = new ArrayList<IJavaElement>();
			for (int i = 0; i < ps.length; i++) {
				if(EclipseResourceUtil.getJavaProject(ps[i]) != null) l.add(JavaCore.create(ps[i]));
			}
			elements = l.toArray(new IJavaElement[0]);
		}
		return SearchEngine.createJavaSearchScope(elements);
	}

	public Object callExternal(Shell shell) {
		if(propertyEditor instanceof JavaEclipseChoicerEditor) {
			return ((JavaEclipseChoicerEditor)propertyEditor).callExternal(shell);
		}
		return null;
	}

}
