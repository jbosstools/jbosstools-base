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

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.jboss.tools.common.model.ui.attribute.adapter.IModelPropertyEditorAdapter;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.internal.ui.refactoring.contentassist.JavaTypeCompletionProcessor;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.util.AccessibleJava;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.util.ModelFeatureFactory;
import org.jboss.tools.common.model.util.XModelObjectUtil;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.templates.ControlContentAssistHelper;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;
import org.jboss.tools.common.model.ui.wizards.NewClassWizard;
import org.jboss.tools.common.model.ui.wizards.NewTypeWizardAdapter;

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
						IProject project = (IProject)xmo.getModel().getProperties().get("project");
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
				setChangeButtonText(JFaceResources.getString("openBrowse"));
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

				NewClassWizard wizard = new NewClassWizard(wizardAdapter);
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
                } catch (Exception e) {
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
        IPackageFragmentRoot root = (project == null) ? null : JavaAdapter.getInstance().getPackageFragmentRoot(project);
        if (root != null) {
    		String pkg = null;
    		if(cpp != null) {
    			pkg = cpp.getContextPackage();
    		}
    		if(pkg == null) pkg = "";
    		JavaTypeCompletionProcessor contentAssistentProcessor = new JavaTypeCompletionProcessor(false, false, pkg.length() == 0);
       		IPackageFragment currentPackage = root.getPackageFragment(pkg);
       		contentAssistentProcessor.setPackageFragment(currentPackage);
            Text text = getTextField();
            text.setData("JavaHyperlinkLineFieldEditor", this);
    		ControlContentAssistHelper.createTextContentAssistant(getTextField(), contentAssistentProcessor, JavaHyperlinkCueLabelProvider.INSTANCE);
        }
		return control;
	}
	
	boolean classExists;
	String textCache = null;
	long timeStamp = -1;
	
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
		String cls = a.getProperty("contextPackageProvider");
		if(cls == null || cls.length() == 0) return null;
		try {
			AccessibleJava.IContextPackageProvider o = (AccessibleJava.IContextPackageProvider)ModelFeatureFactory.getInstance().createFeatureInstance(cls);
			if(o != null) o.setObject(((DefaultValueAdapter)adapter).getModelObject());
			return o;
		} catch (Exception e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}		
		return null;
	}
}
