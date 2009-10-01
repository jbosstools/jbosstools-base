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

import org.eclipse.jface.fieldassist.IContentProposal;
import org.eclipse.jface.fieldassist.IContentProposalProvider;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.action.XEntityData;
import org.jboss.tools.common.model.XModelObject;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
public class PropertiesContentProposalProvider implements IContentProposalProvider {
	protected XModelObject object;
	protected XEntityData data;
	protected XAttribute attribute;	

	public PropertiesContentProposalProvider() {}

	public IContentProposal[] getProposals(String contents, int position) {
		return null;
	}

}
