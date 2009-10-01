/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.attribute.adapter;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.fieldassist.ContentProposalAdapter;
import org.eclipse.jface.fieldassist.IContentProposalProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.action.XEntityData;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.impl.FileAnyImpl;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.attribute.IAttributeContentProposalProvider;

public class PropertiesContentAssistProvider implements
		IAttributeContentProposalProvider {
	XModelObject object;
	XEntityData data;
	XAttribute attribute;

	String fileName = null;
	String entity = null;

	public PropertiesContentAssistProvider() {}

	public boolean isRelevant(XModelObject object, XAttribute attribute) {
		if(object == null || attribute == null) return false;
		if("Property".equals(attribute.getModelEntity().getName())) return true;
		if("HibConfig3Property".equals(attribute.getModelEntity().getName())) return true;
		return false;
	}

	public void init(XModelObject object, XEntityData data, XAttribute attribute) {
		this.object = object;
		this.data = data;
		this.attribute = attribute;
		entity = attribute.getModelEntity().getName();
		fileName = null;

		XModelObject f = object;
		while(f != null && f.getFileType() != XModelObject.FILE) f = f.getParent();
		if(f == null) return;
		fileName = FileAnyImpl.toFileName(f);
		
	}

	public IContentProposalProvider getContentProposalProvider() {
		if(fileName == null) return null;
		PropertiesContentProposalProvider provider = createProcessorByFileName(fileName, entity);
		return provider;
	}

	public LabelProvider getCustomLabelProbider() {
		return null;
	}

	public int getProposalAcceptanceStyle() {
		return ContentProposalAdapter.PROPOSAL_REPLACE;
	}

	public void dispose() {
		
	}

	static String EXTENSION_POINT = "org.jboss.tools.common.model.ui.propertiesFileContentAssist";
	
	private PropertiesContentProposalProvider createProcessorByFileName(String fileName, String entity) {
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint(EXTENSION_POINT);
		if(point == null) return null;
		IConfigurationElement[] cs = point.getConfigurationElements();
		for (IConfigurationElement c: cs) {
			if(fileName.equals(c.getAttribute("fileName"))) {
				try {
					PropertiesContentProposalProvider p = (PropertiesContentProposalProvider)c.createExecutableExtension("attributeProcessor");
					p.object = object;
					p.data = data;
					p.attribute = attribute;
					return p;
				} catch (CoreException e) {
					ModelUIPlugin.getPluginLog().logError(e);
				} catch (ClassCastException e2) {
					ModelUIPlugin.getPluginLog().logError(e2);
				}
			}			
		}

		return null;
	}
}
