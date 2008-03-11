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
import org.eclipse.jdt.internal.ui.refactoring.contentassist.*;
import org.eclipse.jface.contentassist.*;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.swt.graphics.Image;
import org.eclipse.wst.sse.ui.internal.contentassist.*;
import org.eclipse.wst.xml.ui.internal.contentassist.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public class DefaultTreeSelectionContentAssistProcessor extends JavaPackageCompletionProcessor implements ISubjectControlContentAssistProcessor {
	DefaultXAttributeTreeContentProvider treeProvider;
	
	public DefaultTreeSelectionContentAssistProcessor() {}
	
	public void setTreeProvider(DefaultXAttributeTreeContentProvider treeProvider) {
		this.treeProvider = treeProvider;
	}
	
	public ICompletionProposal[] computeCompletionProposals(IContentAssistSubjectControl contentAssistSubjectControl, int documentOffset) {
		List<ICompletionProposal> proposals = new ArrayList<ICompletionProposal>();
		IDocument document = contentAssistSubjectControl.getDocument();
		String text = document.get();
		addAttributeValueProposals(proposals, text, documentOffset);
		return proposals.toArray(new ICompletionProposal[0]);
	}

	public void addAttributeValueProposals(List<ICompletionProposal> proposalsList, String text, int offset) {
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
			int replacementBeginPosition = 0; 
			int replacementLength = text.length();
			int cursorPosition = replacementString.length();
			if(text.startsWith(replacementString)) {
				replacementString = "";
				replacementLength = 0;
			}
			CustomCompletionProposal proposal = new CustomCompletionProposal(
//    				autoactivation, 
					replacementString,
					replacementBeginPosition, 
					replacementLength, 
					cursorPosition, 
					pathsMap.get(paths[i]),
					label, 
					null, 
					null, //contextInfo 
					XMLRelevanceConstants.R_XML_ATTRIBUTE_VALUE);
			proposalsList.add(proposal);
		}
	}
	
	void collectPaths(String startText, XModelObject object, XFilteredTree tree, Map<String,Image> map) {
		String value = tree.getValue(object);
		if(value != null && value.startsWith(startText)) {
			if(value.length() > 0) {
				if(value.equals(startText) && value.endsWith("/")) {
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

}
