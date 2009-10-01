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

import org.eclipse.jface.fieldassist.ContentProposalAdapter;
import org.eclipse.jface.fieldassist.IContentProposalProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.action.XEntityData;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.impl.FileAnyImpl;
import org.jboss.tools.common.model.ui.attribute.IAttributeContentProposalProvider;

public class PropertiesContentAssistProvider implements
		IAttributeContentProposalProvider {
	XModelObject object;
	XEntityData data;
	XAttribute attribute;

	public PropertiesContentAssistProvider() {}

	public boolean isRelevant(XModelObject object, XAttribute attribute) {
		if(object == null || attribute == null) return false;
		if("Property".equals(attribute.getModelEntity().getName())) return true;
		return false;
	}

	public void init(XModelObject object, XEntityData data, XAttribute attribute) {
		this.object = object;
		this.data = data;
		this.attribute = attribute;

		XModelObject f = object;
		while(f != null && f.getFileType() != XModelObject.FILE) f = f.getParent();
		if(f == null) return;
		String fileName = FileAnyImpl.toFileName(f);
		
	}

	public IContentProposalProvider getContentProposalProvider() {
		// TODO Auto-generated method stub
		return null;
	}

	public LabelProvider getCustomLabelProbider() {
		return null;
	}

	public int getProposalAcceptanceStyle() {
		return ContentProposalAdapter.PROPOSAL_REPLACE;
	}

	public void dispose() {
		
	}

}
