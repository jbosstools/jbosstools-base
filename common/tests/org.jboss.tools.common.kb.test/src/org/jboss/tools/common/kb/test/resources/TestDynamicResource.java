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
package org.jboss.tools.common.kb.test.resources;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;

import org.jboss.tools.common.kb.KbDinamicResource;
import org.jboss.tools.common.kb.KbIcon;
import org.jboss.tools.common.kb.KbProposal;

abstract public class TestDynamicResource implements KbDinamicResource {

	abstract public String[] getProposalLabels();

	public void clearConstraints() {
	}

	public Collection<KbProposal> queryProposal(String query) {
		Collection<KbProposal> proposals = new ArrayList<KbProposal>();

		for(int i=0; i<getProposalLabels().length; i++) {
			String bundle = getProposalLabels()[i];
			if (bundle.toLowerCase().startsWith(query.toLowerCase())) {
				KbProposal proposal = new KbProposal();
				proposal.setLabel(bundle);
				proposal.setReplacementString(bundle);
				proposal.setIcon(KbIcon.ENUM_ITEM);
				proposal.setPosition(bundle.length());

				proposals.add(proposal);
			}
		}
		return proposals;
	}

	public void setConstraint(String name, String value) {
	}

	public InputStream getInputStream() {
		return null;
	}
}