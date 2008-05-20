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
import org.jboss.tools.common.model.ui.IValueChangeListener;
import org.jboss.tools.common.model.ui.IValueProvider;
import org.jboss.tools.common.model.ui.attribute.adapter.DefaultValueAdapter;
import org.eclipse.jdt.core.*;
import org.eclipse.jdt.internal.ui.refactoring.contentassist.JavaTypeCompletionProcessor;
import org.eclipse.swt.widgets.*;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.util.XModelObjectUtil;
import org.jboss.tools.common.model.ui.templates.ControlContentAssistHelper;

public class JavaHyperlinkCellEditor extends DialogCellEditorEx 
		implements JavaHyperlinkCueLabelProvider.JavaClassHolder {
	private IProject project;
	private String publicId;
	private String xPath;
	
	public JavaHyperlinkCellEditor() {}

	public JavaHyperlinkCellEditor(Composite parent) {
		super(parent);
	}
	
	public JavaHyperlinkCellEditor(Composite parent, int style) {
		super(parent, style);
	}

	public void setPropertyEditor(PropertyEditor propertyEditor) {
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
					}
				}
			}
//			valueProvider.addValueChangeListener(this);
			valueChangeListener = (IValueChangeListener)propertyEditor.getAdapter(IValueChangeListener.class);
			if(project != null) {
//				setLabelAction(new OpenJavaEditorAction());
//				setSelectableLabel(Boolean.TRUE.booleanValue());
//				setChangeButtonText(JFaceResources.getString("openBrowse"));
			}

	        IPackageFragmentRoot root = (project == null) ? null : JavaAdapter.getInstance().getPackageFragmentRoot(project);
	        if (root != null) {
	    		JavaTypeCompletionProcessor contentAssistentProcessor = new JavaTypeCompletionProcessor(false, false);
	       		IPackageFragment currentPackage = root.getPackageFragment("");
	       		contentAssistentProcessor.setPackageFragment(currentPackage);
	            Text text = getTextField();
	            text.setData("JavaHyperlinkLineFieldEditor", this);
	    		ControlContentAssistHelper.createTextContentAssistant(getTextField(), contentAssistentProcessor, JavaHyperlinkCueLabelProvider.INSTANCE);
	        }
		}
	}

	protected Text getTextField() {
		return text;
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
	
}
