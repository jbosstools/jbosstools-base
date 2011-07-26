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
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.PartInitException;
import org.jboss.tools.common.CommonPlugin;
import org.jboss.tools.common.el.core.ELCorePlugin;
import org.jboss.tools.common.el.core.parser.LexicalToken;
import org.jboss.tools.common.el.core.resolver.TypeInfoCollector.MemberInfo;

/**
 * @author Alexey Kazakov
 */
public class JavaMemberELSegmentImpl extends ELSegmentImpl implements JavaMemberELSegment {

	protected IJavaElement element;
	protected Set<IJavaElement> allElements;
	protected MemberInfo memberInfo;
	protected boolean hasSetter;
	protected boolean hasGetter;
	protected Map<String, TypeInfoCollector.MethodInfo> unpairedGettersOrSetters;

	public JavaMemberELSegmentImpl(LexicalToken token) {
		super(token);
	}

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
		try {
			setResource(element.getUnderlyingResource());
		} catch (JavaModelException e) {
			ELCorePlugin.getDefault().logError(e);
		}
	}

	/**
	 * Adds a Java Element for the Segment
	 * 
	 * @param element
	 */
	public void addJavaElement(IJavaElement element) {
		if (this.allElements == null) {
			this.allElements = new HashSet<IJavaElement>();
		}
		this.allElements.add(element);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.JavaMemberELSegment#getAllJavaElements()
	 */
	public IJavaElement[] getAllJavaElements() {
		if (this.allElements == null || this.allElements.size() == 0) {
			return new IJavaElement[0];
		}
		return (IJavaElement[])this.allElements.toArray(new IJavaElement[this.allElements.size()]);
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

	public IOpenableReference[] getOpenable() {
		if(getJavaElement() != null && getJavaElement().exists()) {
			IOpenableReference openable = new IOpenableReference() {
				@Override
				public boolean open() {
					try {
						return JavaUI.openInEditor(getJavaElement()) != null;
					} catch (PartInitException e) {
						CommonPlugin.getDefault().logError(e);
					} catch (JavaModelException e) {
						CommonPlugin.getDefault().logError(e);
					}
					return false;
				}
				
				@Override
				public String getLabel() {
					return getJavaElement().getElementName();
				}
				
				@Override
				public Image getImage() {
					return null;
				}
			};
			return new IOpenableReference[]{openable};
		}
		return new IOpenableReference[0];
	}

}