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
package org.jboss.tools.common.model.ui.attribute.adapter;

import java.util.*;
import org.eclipse.jface.fieldassist.ContentProposalAdapter;
import org.eclipse.jface.fieldassist.IContentProposal;
import org.eclipse.jface.fieldassist.IContentProposalProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.action.XEntityData;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.ui.attribute.AttributeContentProposalProviderFactory;
import org.jboss.tools.common.model.ui.attribute.IAttributeContentProposalProvider;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public class DefaultTreeSelectionContentAssistProvider implements IAttributeContentProposalProvider {
	XModelObject object;
	XAttribute attribute;
	
	DefaultXAttributeTreeContentProvider treeProvider;
	
	public DefaultTreeSelectionContentAssistProvider() {}
	
	public boolean isRelevant(XModelObject object, XAttribute attribute) {
		if(object == null) {
			//TODO it looks like we need to pass model, if we do not have object
			return false;
		}
		String editor = attribute.getEditor().getName();
		return editor != null && editor.startsWith("Tree"); //$NON-NLS-1$
	}

	void addAttributeValueProposals(List<IContentProposal> proposalsList, String text, int offset) {
		if(treeProvider == null) return;
		XFilteredTree tree = treeProvider.getFilteredTree();
		if(tree == null) {
			treeProvider.getElements(null);
			tree = treeProvider.getFilteredTree();
		}
		if(tree == null) return;
		XModelObject root = tree.getRoot();
		if(root == null) return;
		String startText = text.substring(0, offset);
		Map<String,Image> pathsMap = new TreeMap<String,Image>();
		collectPaths(startText, root, tree, pathsMap);
		String[] paths = pathsMap.keySet().toArray(new String[0]);
		if(paths.length == 1 && text.startsWith(paths[0])) {
			pathsMap = new TreeMap<String,Image>();
			collectPaths(paths[0], root, tree, pathsMap);
			paths = (String[])pathsMap.keySet().toArray(new String[0]);
		}
		for (int i = 0; i < paths.length; i++) {
			String label = paths[i];
			String replacementString = paths[i];
			if(text.startsWith(replacementString)) {
//				replacementString = "";
			}
			
			IContentProposal proposal = AttributeContentProposalProviderFactory.makeContentProposal(replacementString, replacementString);
				 
			proposalsList.add(proposal);
		}
	}
	
	void collectPaths(String startText, XModelObject object, XFilteredTree tree, Map<String,Image> map) {
		String value = tree.getValue(object);
		if(value != null && value.startsWith(startText)) {
			if(value.length() > 0) {
				if(value.equals(startText) && value.endsWith("/")) { //$NON-NLS-1$
					//skip this value 
				} else {
					map.put(value, EclipseResourceUtil.getImage(object));
				}
			}
			if(value.length() > startText.length()) return;
		} else if(value != null && !startText.startsWith(value)) {
			return;
		}		
		XModelObject[] cs = tree.getChildren(object);
		for (int i = 0; i < cs.length; i++) collectPaths(startText, cs[i], tree, map);
	}

	public IContentProposalProvider getContentProposalProvider() {
		return new ContentProposalProvider();
	}

	public int getProposalAcceptanceStyle() {
		return ContentProposalAdapter.PROPOSAL_REPLACE;
	}

	public void init(XModelObject object, XEntityData data, XAttribute attribute) {
		this.object = object;
		this.attribute = attribute;
		treeProvider = new DefaultXAttributeTreeContentProvider(attribute, object.getModel(), object);
	}

	public LabelProvider getCustomLabelProbider() {
		return null;
	}

	public void dispose() {
		object = null;
		attribute = null;
		treeProvider = null;
	}

	class ContentProposalProvider implements IContentProposalProvider {

		public IContentProposal[] getProposals(String contents, int position) {
			List<IContentProposal> proposalsList = new ArrayList<IContentProposal>();
			addAttributeValueProposals(proposalsList, contents, position);
			return proposalsList.toArray(new IContentProposal[0]);
		}
		
	}

}
