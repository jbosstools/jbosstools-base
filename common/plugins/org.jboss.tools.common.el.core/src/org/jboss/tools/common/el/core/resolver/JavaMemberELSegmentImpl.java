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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jdt.core.IJavaElement;
import org.jboss.tools.common.el.core.resolver.TypeInfoCollector.MemberInfo;

/**
 * @author Alexey Kazakov
 */
public class JavaMemberELSegmentImpl extends ELSegmentImpl implements JavaMemberELSegment {

	protected IJavaElement element;
	protected MemberInfo memberInfo;
	protected boolean hasSetter;
	protected boolean hasGetter;
	protected Map<String, TypeInfoCollector.MethodInfo> unpairedGettersOrSetters;

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.JavaMemberElSegment#getJavaElement()
	 */
	public IJavaElement getJavaElement() {
		if(element==null && memberInfo!=null) {
			element = memberInfo.getJavaElement();
		}
		return element;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.JavaMemberElSegment#getMemberInfo()
	 */
	public MemberInfo getMemberInfo() {
		return memberInfo;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.JavaMemberElSegment#hasGetter()
	 */
	public boolean hasGetter() {
		return hasGetter;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.JavaMemberElSegment#hasSetter()
	 */
	public boolean hasSetter() {
		return hasSetter;
	}

	/**
	 * @param element the element to set
	 */
	public void setElement(IJavaElement element) {
		this.element = element;
	}

	/**
	 * @return the hasSetter
	 */
	public boolean isHasSetter() {
		return hasSetter;
	}

	/**
	 * @param hasSetter the hasSetter to set
	 */
	public void setHasSetter(boolean hasSetter) {
		this.hasSetter = hasSetter;
	}

	/**
	 * @return the hasGetter
	 */
	public boolean isHasGetter() {
		return hasGetter;
	}

	/**
	 * @param hasGetter the hasGetter to set
	 */
	public void setHasGetter(boolean hasGetter) {
		this.hasGetter = hasGetter;
	}

	/**
	 * @param memberInfo the memberInfo to set
	 */
	public void setMemberInfo(MemberInfo memberInfo) {
		this.memberInfo = memberInfo;
		this.setElement(memberInfo != null ? memberInfo.getJavaElement() : null);
	}

	/**
	 * @return Map of unpaired getters and setters (getters/setters without proper setters/getters).
	 * of all properties used in EL.
	 * Key - name of property.
	 * Value - MethodInfo of existed getter/setter.
	 */
	public Map<String, TypeInfoCollector.MethodInfo> getUnpairedGettersOrSetters() {
		if (unpairedGettersOrSetters == null) {
			unpairedGettersOrSetters = new HashMap<String, TypeInfoCollector.MethodInfo>();
		}
		return unpairedGettersOrSetters;
	}

	/**
	 * Clear Map of unpaired getters and setters.
	 */
	public void clearUnpairedGettersOrSetters() {
		getUnpairedGettersOrSetters().clear();
	}
}