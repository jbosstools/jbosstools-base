package org.jboss.tools.common.model.ui.attribute;

import org.eclipse.jface.fieldassist.IContentProposalProvider;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.model.XModelObject;

public interface IAttributeContentProposalProvider {

	public void init(XModelObject object, XAttribute attribute);

	public IContentProposalProvider getContentProposalProvider();
	
	/**
	 * ContentProposalAdapter.PROPOSAL_INSERT
	 * ContentProposalAdapter.PROPOSAL_REPLACE
	 * @return
	 */
	public int getProposalAcceptanceStyle();

	public void dispose();
	
}
