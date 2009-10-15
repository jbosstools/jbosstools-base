/******************************************************************************* 
 * Copyright (c) 2009 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.el.core.resolver;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.jdt.core.IJavaElement;
import org.jboss.tools.common.el.core.model.ELExpression;
import org.jboss.tools.common.el.core.model.ELInvocationExpression;
import org.jboss.tools.common.text.TextProposal;

/**
 * @author Alexey Kazakov
 */
public class ELResolutionImpl implements ELResolution {

	protected ELContext context;
	protected List<ELSegment> segments = new ArrayList<ELSegment>();
	protected ELExpression operand;
	protected Set<TextProposal> proposals = new HashSet<TextProposal>();
	protected ELInvocationExpression lastResolvedToken;
	protected boolean mapOrCollectionOrBundleAmoungTheTokens;
	
	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELResolution#findSegmentsByJavaElement(org.eclipse.jdt.core.IJavaElement)
	 */
	public List<ELSegment> findSegmentsByVariable(IVariable variable) {
		ArrayList<ELSegment> list = new ArrayList<ELSegment>();
		for(ELSegment segment : segments){
			for(IVariable var : segment.getVariables()){
				if(var.getName().equals(variable.getName())){
					list.add(segment);
				}
			}
		}
		return list;
	}


	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELResolution#findSegmentsByJavaElement(org.eclipse.jdt.core.IJavaElement)
	 */
	public List<ELSegment> findSegmentsByJavaElement(IJavaElement element) {
		ArrayList<ELSegment> list = new ArrayList<ELSegment>();
		for(ELSegment segment : segments){
			if(segment instanceof JavaMemberELSegment && ((JavaMemberELSegment)segment).getJavaElement() != null && ((JavaMemberELSegment)segment).getJavaElement().equals(element))
				list.add(segment);
		}
		return list;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELResolution#findSegmentByOffset(int)
	 */
	public ELSegment findSegmentByOffset(int offset) {
		for(ELSegment segment : segments){
			if(segment.getSourceReference() != null && segment.getSourceReference().getStartPosition() == offset)
				return segment;
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELResolution#getContext()
	 */
	public ELContext getContext() {
		return context;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELResolution#getSegments()
	 */
	public List<ELSegment> getSegments() {
		return segments;
	}

	/**
	 * Adds a segment
	 * @param segment
	 */
	public void addSegment(ELSegment segment) {
		segments.add(segment);
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELResolution#getSourceOperand()
	 */
	public ELExpression getSourceOperand() {
		return operand;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELResolution#getUnresolvedSegment()
	 */
	public ELSegment getUnresolvedSegment() {
		for (ELSegment segment : segments) {
			if(!segment.isResolved()) {
				return segment;
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELResolution#isResolved()
	 */
	public boolean isResolved() {
		return !segments.isEmpty() && getUnresolvedSegment()==null;
	}

	/**
	 * @return the proposals
	 */
	public Set<TextProposal> getProposals() {
		return proposals;
	}

	/**
	 * @param proposals the proposals to set
	 */
	public void setProposals(Set<TextProposal> proposals) {
		this.proposals = proposals;
	}

	/**
	 * @param operand the operand to set
	 */
	public void setSourceOperand(ELExpression operand) {
		this.operand = operand;
	}

	/**
	 * @param context the context to set
	 */
	public void setContext(ELContext context) {
		this.context = context;
	}

	/**
	 * @param segments the segments to set
	 */
	public void setSegments(List<ELSegment> segments) {
		this.segments = segments;
	}

	/**
	 * @return the lastResolvedToken
	 */
	public ELInvocationExpression getLastResolvedToken() {
		return lastResolvedToken;
	}

	/**
	 * @param lastResolvedToken the lastResolvedToken to set
	 */
	public void setLastResolvedToken(ELInvocationExpression lastResolvedToken) {
		this.lastResolvedToken = lastResolvedToken;
	}

	/**
	 * @return the operand
	 */
	public ELExpression getOperand() {
		return operand;
	}

	/**
	 * @param operand the operand to set
	 */
	public void setOperand(ELExpression operand) {
		this.operand = operand;
	}

	/**
	 * @return the mapOrCollectionOrBundleAmoungTheTokens
	 */
	public boolean isMapOrCollectionOrBundleAmoungTheTokens() {
		return mapOrCollectionOrBundleAmoungTheTokens;
	}

	/**
	 * @param mapOrCollectionOrBundleAmoungTheTokens the mapOrCollectionOrBundleAmoungTheTokens to set
	 */
	public void setMapOrCollectionOrBundleAmoungTheTokens(
			boolean mapOrCollectionOrBundleAmoungTheTokens) {
		this.mapOrCollectionOrBundleAmoungTheTokens = mapOrCollectionOrBundleAmoungTheTokens;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELResolution#getLastSegment()
	 */
	public ELSegment getLastSegment() {
		if(!getSegments().isEmpty()) {
			return getSegments().get(getSegments().size()-1);
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELResolution#getNumberOfResolvedSegments()
	 */
	public int getNumberOfResolvedSegments() {
		int i = 0;
		for (ELSegment segment : segments) {
			if(segment.isResolved()) {
				i++;
			} else {
				break;
			}
		}
		return i;
	}
}