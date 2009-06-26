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
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class EditorPartFactory {
	IConfigurationElement element;
	String id;
	Class editorClass;
	Class contributorClass;
	
	EditorPartFactory(IConfigurationElement element, Class editorClass, Class contributorClass) throws InstantiationException, IllegalAccessException {
		this.element = element;
		this.editorClass = editorClass;
		this.contributorClass = contributorClass;
		if(!IEditorPart.class.isAssignableFrom(editorClass))
		  throw new ClassCastException("Class " + editorClass + " must be instance of org.eclipse.ui.IEditorPart"); //$NON-NLS-1$ //$NON-NLS-2$
		if(!IEditorActionBarContributor.class.isAssignableFrom(contributorClass))
		  throw new ClassCastException("Class " + contributorClass.getName() + " must be instance of org.eclipse.ui.IEditorActionBarContributor"); //$NON-NLS-1$ //$NON-NLS-2$
		editorClass.newInstance();
		contributorClass.newInstance();
		id = editorClass.getName();
	}
	
	public IEditorPart createEditorPart() {
		IEditorPart part = null;
			try {
				part = (IEditorPart)editorClass.newInstance();
			} catch (InstantiationException e) {
				ModelUIPlugin.getPluginLog().logError(e);
			} catch (IllegalAccessException e) {
				ModelUIPlugin.getPluginLog().logError(e);
			}
			return part;
	}
	
	public IEditorActionBarContributor createEditorActionBarContributor() {
		IEditorActionBarContributor contributor = null;
		try {
			contributor = (IEditorActionBarContributor)contributorClass.newInstance();
		} catch (InstantiationException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		} catch (IllegalAccessException e) {
			ModelUIPlugin.getPluginLog().logError(e);
		}
		return contributor;
	}
	
	public IConfigurationElement getConfigurationElement() {
		return element;
	}
	
	public String getEditorId() {
		return id;
	}

}
