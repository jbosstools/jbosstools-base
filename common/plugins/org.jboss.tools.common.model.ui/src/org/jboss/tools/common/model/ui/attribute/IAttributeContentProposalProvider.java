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
package org.jboss.tools.common.model.ui.attribute;

import org.eclipse.jface.fieldassist.IContentProposalProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.action.XEntityData;
import org.jboss.tools.common.model.XModelObject;

/**
 * @author Viacheslav Kabanovich
 */
public interface IAttributeContentProposalProvider {

	public boolean isRelevant(XModelObject object, XAttribute attribute);

	public void init(XModelObject object, XEntityData data, XAttribute attribute);

	public IContentProposalProvider getContentProposalProvider();

	public LabelProvider getCustomLabelProbider();
	
	/**
	 * ContentProposalAdapter.PROPOSAL_INSERT
	 * ContentProposalAdapter.PROPOSAL_REPLACE
	 * @return
	 */
	public int getProposalAcceptanceStyle();

	public void dispose();
	
}
