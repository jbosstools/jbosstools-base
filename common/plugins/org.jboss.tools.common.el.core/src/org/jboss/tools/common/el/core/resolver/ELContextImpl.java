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
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.jboss.tools.common.el.core.ELReference;

/**
 * EL context
 * @author Alexey Kazakov
 */
public class ELContextImpl extends SimpleELContext {
	public static final List<Var> EMPTY = Collections.<Var>emptyList();

	protected List<Var> allVars = new ArrayList<Var>();
	protected ELReference[] elReferences;
	protected List<ELReference> elReferenceSet;

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELContext#getVars()
	 */
	@Override
	public synchronized Var[] getVars() {
		List<Var> vars = getVarsAsList();
		return vars.toArray(new Var[vars.size()]);
	}

	@Override
	public synchronized List<Var> getVarsAsList() {
		List<Var> external = getExternalVars();
		if(external.isEmpty()) {
			return allVars;
		} else if(allVars.isEmpty()) {
			return external;
		}
		ArrayList<Var> result = new ArrayList<Var>();
		result.addAll(allVars);
		result.addAll(external);
		return result;
	}

	/**
	 * Returns list of vars that are not defined withing current page, 
	 * for instance, by <ui:param> on a page that includes this page.
	 *  
	 * @return
	 */
	public List<Var> getExternalVars() {
		return EMPTY;
	}

	/**
	 * Adds new Var to the context
	 * @param region
	 * @param vars
	 */
	public synchronized void addVar(Region region, Var var) {
		var.setRegion(region);
		allVars.add(var);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELContext#getVars(int)
	 */
	@Override
	public synchronized Var[] getVars(int offset) {
		List<Var> vars = getVarsAsList(offset);
		return vars.toArray(new Var[vars.size()]);
	}

	@Override
	public synchronized List<Var> getVarsAsList(int offset) {
		if(offset < 0) {
			return getVarsAsList();
		}
		List<Var> external = getExternalVars();
		if(allVars.isEmpty()) {
			return external;
		}
		List<Var> result = new ArrayList<Var>();
		for (Var var : allVars) {
			Region region = var.getRegion();
			if(offset>=region.getOffset() && offset<=region.getOffset() + region.getLength()) {
				result.add(var);
			}
		}
		if(!external.isEmpty()) {
			result.addAll(external);
		}
		return result;
	}

	/**
	 * @return the allVars
	 */
	public List<Var> getAllVars() {
		return allVars;
	}

	/**
	 * @param allVars the allVars to set
	 */
	public void setAllVars(List<Var> allVars) {
		this.allVars = allVars;
	}

	/**
	 * @see org.jboss.tools.jst.web.kb.IXmlContext#getELReferences()
	 * 
	 * Implementation returns sorted array of EL references.
	 */
	@Override
	public synchronized ELReference[] getELReferences() {
		if(elReferences==null) {
			if(elReferenceSet==null || elReferenceSet.isEmpty()) {
				return elReferences = EMPTY_ARRAY;
			}
			elReferences = elReferenceSet.toArray(new ELReference[0]);
			sortELReferences();
		}
		return elReferences;
	}

	public synchronized void addELReference(ELReference reference) {
		if(elReferenceSet==null) {
			elReferenceSet = new ArrayList<ELReference>();
		}
		elReferenceSet.add(reference);
		elReferences = null;
	}

	/**
	 * @see org.jboss.tools.common.el.core.resolver.SimpleELContext#getELReference(int)
	 * 
	 * Implementation uses binary search in sorted array of EL references.
	 */
	@Override
	public synchronized ELReference getELReference(int offset) {
		getELReferences();
		if(elReferences.length == 0) {
			return null;
		} else if(elReferences.length == 1) {
			ELReference ref = elReferences[0];
			if(ref.getStartPosition()<=offset && (ref.getStartPosition() + ref.getLength()>offset)) {
				return ref;
			}
		} else {
			int left = 0;
			ELReference ref = elReferences[left];
			if(ref.getStartPosition() > offset) return null;
			if(ref.getStartPosition() + ref.getLength() >= offset) return ref;
			int right = elReferences.length - 1;
			ref = elReferences[right];
			if(ref.getStartPosition() + ref.getLength() < offset) return null;
			if(ref.getStartPosition() <= offset) return ref;
			while(right - left > 1) {
				int middle = (right + left) / 2;
				ref = elReferences[middle];
				if(ref.getStartPosition()<=offset && (ref.getStartPosition() + ref.getLength()>offset)) {
					return ref;
				} else if(ref.getStartPosition() > offset) {
					right = middle;
				} else {
					left = middle;
				}
			}
			return null;
		}
		return null;
	}

	/**
	 * Implementation first uses binary search in sorted array of EL references 
	 * for the left and right borders of the region, then checks and collects 
	 * all EL references out of the defined range.
	 */
	@Override
	public synchronized Collection<ELReference> getELReferences(IRegion region) {
		List<ELReference> references = new ArrayList<ELReference>();
		if(elReferenceSet == null || elReferenceSet.isEmpty()) {
			return references;
		}
		getELReferences();
		int min = getMin(region.getOffset());
		int max = getMax(region.getOffset() + region.getLength());
		if(min >= 0 && max >= min) {
			for (int i = min; i <= max; i++) {
				ELReference ref = elReferences[i];
				if(region.getOffset() + region.getLength() >= ref.getStartPosition() && region.getOffset() <= ref.getStartPosition() + ref.getLength()) {
					references.add(ref);
				}
			}
		}
		return references;
	}

	/**
	 * Returns index of an EL reference such that all previous references lie to the left of offset 
	 * and all subsequent references either contain the offset or lie to the right of it.
	 * The EL reference defined by the returned index, may either contain the offset or not.
	 *   
	 * This method is called by getELReferences(IRegion) with offset of the left border of the region.
	 *  
	 * @param offset
	 * @return
	 */
	private int getMin(int offset) {
		if(elReferences.length == 0) {
			return -1;
		} else if(elReferences.length == 1) {
			ELReference ref = elReferences[0];
			if(ref.getStartPosition()<=offset && (ref.getStartPosition() + ref.getLength()>offset)) {
				return 0;
			}
		} else {
			int left = 0;
			ELReference ref = elReferences[left];
			if(ref.getStartPosition() + ref.getLength() >= offset) return left;
			int right = elReferences.length - 1;
			ref = elReferences[right];
			if(ref.getStartPosition() + ref.getLength() < offset) return -1;
			if(ref.getStartPosition() <= offset) return right;
			while(right - left > 1) {
				int middle = (right + left) / 2;
				ref = elReferences[middle];
				if(ref.getStartPosition()<=offset && (ref.getStartPosition() + ref.getLength()>offset)) {
					return middle;
				} else if(ref.getStartPosition() + ref.getLength() < offset) {
					left = middle + 1;
				} else {
					right = middle;
				}
			}
			return left;
		}
		return -1;
	}

	/**
	 * Returns index of an EL reference such that all subsequent references lie to the rigth of offset 
	 * and all previous references either contain the offset or lie to the left of it.
	 * The EL reference defined by the returned index, may either contain the offset or not.
	 * 
	 * This method is called by getELReferences(IRegion) with offset of the right border of the region.
	 *  
	 * @param offset
	 * @return
	 */
	private int getMax(int offset) {
		if(elReferences.length == 0) {
			return -1;
		} else if(elReferences.length == 1) {
			ELReference ref = elReferences[0];
			if(ref.getStartPosition() <= offset && (ref.getStartPosition() + ref.getLength()>offset)) {
				return 0;
			}
		} else {
			int left = 0;
			ELReference ref = elReferences[left];
			if(ref.getStartPosition() > offset) return -1;
			if(ref.getStartPosition() + ref.getLength() >= offset) return left;
			int right = elReferences.length - 1;
			ref = elReferences[right];
			if(ref.getStartPosition() <= offset) return right;
			while(right - left > 1) {
				int middle = (right + left) / 2;
				ref = elReferences[middle];
				if(ref.getStartPosition()<=offset && (ref.getStartPosition() + ref.getLength()>offset)) {
					return middle;
				} else if(ref.getStartPosition() > offset) {
					right = middle - 1;
				} else {
					left = middle;
				}
			}
			return right;
		}
		return -1;
	}

	/**
	 * Implementation sorts and removes duplicated EL references.
	 */
	private void sortELReferences() {
		if(elReferenceSet == null || elReferenceSet.size() < 2) return;
		Arrays.sort(elReferences, new ReferencesComparator());
		List<ELReference> newReferencesSet = new ArrayList<ELReference>();
		newReferencesSet.add(elReferences[0]);
		ELReference last = null;
		for (int i = 0; i < elReferences.length; i++) {
			ELReference next = elReferences[i];
			if(last != null && last.getStartPosition() + last.getLength() >= next.getStartPosition()) {
				continue;
			}
			newReferencesSet.add(next);
			last = next;
		}
		elReferenceSet = newReferencesSet;
		if(elReferenceSet.size() < elReferences.length) {
			elReferences = null;
			getELReferences();
		}
	}
	
	static class ReferencesComparator implements Comparator<ELReference> {

		@Override
		public int compare(ELReference o1, ELReference o2) {
			if(o1.getStartPosition() != o2.getStartPosition()) {
				return o1.getStartPosition() - o2.getStartPosition();
			}
			return o2.getStartPosition() + o2.getLength() - o1.getStartPosition() - o1.getLength();
		}
		
	}
}