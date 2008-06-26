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
package org.jboss.tools.common.model.ui.editor;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.ui.IEditorActionBarContributor;
import org.eclipse.ui.IEditorPart;

public class EditorPartFactory {
	IConfigurationElement element;
	String id;
	Class editorClass;
	Class contributorClass;
	
	EditorPartFactory(IConfigurationElement element, Class editorClass, Class contributorClass) throws Exception {
		this.element = element;
		this.editorClass = editorClass;
		this.contributorClass = contributorClass;
		if(!IEditorPart.class.isAssignableFrom(editorClass))
		  throw new ClassCastException("Class " + editorClass + " must be instance of org.eclipse.ui.IEditorPart");
		if(!IEditorActionBarContributor.class.isAssignableFrom(contributorClass))
		  throw new ClassCastException("Class " + contributorClass.getName() + " must be instance of org.eclipse.ui.IEditorActionBarContributor");
		editorClass.newInstance();
		contributorClass.newInstance();
		id = editorClass.getName();
	}
	
	public IEditorPart createEditorPart() {
		try {
			return (IEditorPart)editorClass.newInstance();
		} catch (Exception e) {
			return null;
		}
	}
	
	public IEditorActionBarContributor createEditorActionBarContributor() {
		try {
			return (IEditorActionBarContributor)contributorClass.newInstance();
		} catch (Exception e) {
			return null;
		}
	}
	
	public IConfigurationElement getConfigurationElement() {
		return element;
	}
	
	public String getEditorId() {
		return id;
	}

}
