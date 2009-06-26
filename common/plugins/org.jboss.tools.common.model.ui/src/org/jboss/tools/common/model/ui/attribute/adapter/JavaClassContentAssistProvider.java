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

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jface.fieldassist.ContentProposalAdapter;
import org.eclipse.jface.fieldassist.IContentProposalProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.pde.internal.ui.editor.contentassist.TypeContentProposalProvider;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.attribute.IAttributeContentProposalProvider;
import org.jboss.tools.common.model.ui.attribute.editor.JavaHyperlinkCueLabelProvider;

public class JavaClassContentAssistProvider implements
		IAttributeContentProposalProvider {
	XModelObject object;
	XAttribute attribute;

	public IContentProposalProvider getContentProposalProvider() {
		IProject project = (IProject)object.getModel().getProperties().get("project"); //$NON-NLS-1$
		return (project == null) ? null : new TypeContentProposalProvider(project, IJavaSearchConstants.TYPE);
	}

	public int getProposalAcceptanceStyle() {
		return ContentProposalAdapter.PROPOSAL_REPLACE;
	}

	public void init(XModelObject object, XAttribute attribute) {
		this.object = object;
		this.attribute = attribute;
	}

	public boolean isRelevant(XModelObject object, XAttribute attribute) {
		if(object == null || attribute == null) return false;
		String editorName = attribute.getEditor().getName();
		return editorName != null && editorName.indexOf("AccessibleJava") >= 0; //$NON-NLS-1$
	}

	public LabelProvider getCustomLabelProbider() {
		return JavaHyperlinkCueLabelProvider.INSTANCE;
	}

	public void dispose() {
		this.object = null;
		this.attribute = null;
	}

}
