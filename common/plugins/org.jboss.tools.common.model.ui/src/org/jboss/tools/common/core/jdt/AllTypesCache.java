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
package org.jboss.tools.common.core.jdt;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jdt.core.ElementChangedEvent;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IElementChangedListener;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaElementDelta;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.core.search.IJavaSearchScope;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.core.search.SearchPattern;
import org.eclipse.jdt.core.search.TypeNameMatch;

public class AllTypesCache {	

	private static TypeNameMatch[] cache = null;
	private static int fgSizeHint= 2000;
	
	private static int fgNumberOfCacheFlushes= 0;

	private static class TypeNameComparator implements Comparator {
		public int compare(Object o1, Object o2) {
			return ((TypeNameMatch)o1).getSimpleTypeName().compareTo(((TypeNameMatch)o2).getSimpleTypeName());
		}
	}
	
	private static Comparator fgTypeNameComparator= new TypeNameComparator();

	/**
	 * Returns all types in the given scope.
	 * @param kind IJavaSearchConstants.CLASS, IJavaSearchConstants.INTERFACE
	 * or IJavaSearchConstants.TYPE
	 * @param typesFound The resulting <code>TypeNameMatch</code> elements are added to this collection
	 */		
	public static void getTypes(IJavaSearchScope scope, int kind, IProgressMonitor monitor, Collection typesFound) throws JavaModelException {
		TypeNameMatch[] ts = getAllTypes(monitor);
		for (int i = 0; i < ts.length; i++) {
			TypeNameMatch info = (TypeNameMatch) cache[i];
			checkType(info, scope, kind, monitor, typesFound);
		}
	}
	private static void checkType(TypeNameMatch info, IJavaSearchScope scope, int kind, IProgressMonitor monitor, Collection typesFound) throws JavaModelException {
		if (scope.equals(SearchEngine.createWorkspaceScope()) || scope.encloses(info.getType())) {
			if (kind == IJavaSearchConstants.TYPE || (kind == IJavaSearchConstants.INTERFACE == info.getType().isInterface())) {
				typesFound.add(info);
			}
		}
	}
	

	/**
	 * Returns all types in the workspace. The returned array must not be
	 * modified. The elements in the array are sorted by simple type name.
	 */
	public static synchronized TypeNameMatch[] getAllTypes(IProgressMonitor monitor) throws JavaModelException { 	
		if (cache == null) {
			int r = loadCache(monitor);
			if(r < 0) return null; else monitor = null;
		}
		if (monitor != null) {
			monitor.done();
		}
		return cache;
	}
	private static int loadCache(IProgressMonitor monitor) throws JavaModelException {
		ArrayList searchResult = new ArrayList(fgSizeHint);
		doSearchTypes(SearchEngine.createWorkspaceScope(), IJavaSearchConstants.TYPE, monitor, searchResult);
		if (monitor != null && monitor.isCanceled()) {
			return -1;
		}
		cache = (TypeNameMatch[]) searchResult.toArray(new TypeNameMatch[searchResult.size()]);
		Arrays.sort(cache, getTypeNameComperator());
		fgSizeHint= cache.length;

		JavaCore.addElementChangedListener(new TypeCacheDeltaListener());
		return 0;
	}
	
	/**
	 * Returns true if the type cache is up to date.
	 */
	public static boolean isCacheUpToDate() {
		return cache != null;
	}
	
	/**
	 * Returns a compartor that compares the simple type names
	 */
	public static Comparator getTypeNameComperator() {
		return fgTypeNameComparator;
	}

	private static void doSearchTypes(IJavaSearchScope scope, int style, IProgressMonitor monitor, Collection typesFound) throws JavaModelException {
		new SearchEngine().searchAllTypeNames(
			null,
			null,
			SearchPattern.R_PATTERN_MATCH,
			style,
			scope,
			new TypeInfoRequestor(typesFound),
			IJavaSearchConstants.WAIT_UNTIL_READY_TO_SEARCH,
			monitor);
	}
	
	private static class TypeCacheDeltaListener implements IElementChangedListener {
		
		/*
		 * @see IElementChangedListener#elementChanged
		 */
		public void elementChanged(ElementChangedEvent event) {
			boolean needsFlushing= processDelta(event.getDelta());
			if (needsFlushing) {
				cache= null;
				fgNumberOfCacheFlushes++;
				JavaCore.removeElementChangedListener(this); // it's ok to remove listener while delta processing
			}
		}
		
		/*
		 * returns true if the cache needs to be flushed
		 */
		private boolean processDelta(IJavaElementDelta delta) {
			IJavaElement elem= delta.getElement();
			boolean isAddedOrRemoved= (delta.getKind() != IJavaElementDelta.CHANGED)
			 || (delta.getFlags() & (IJavaElementDelta.F_ADDED_TO_CLASSPATH | IJavaElementDelta.F_REMOVED_FROM_CLASSPATH)) != 0;
			
			switch (elem.getElementType()) {
				case IJavaElement.JAVA_MODEL:
				case IJavaElement.JAVA_PROJECT:
				case IJavaElement.PACKAGE_FRAGMENT_ROOT:
				case IJavaElement.PACKAGE_FRAGMENT:
				case IJavaElement.CLASS_FILE:
				case IJavaElement.TYPE: // type children can be inner classes
					if (isAddedOrRemoved) {
						return true;
					}				
					return processChildrenDelta(delta);
				case IJavaElement.COMPILATION_UNIT: // content change means refresh from local
					if (((ICompilationUnit) elem).isWorkingCopy()) {
						return false;
					}
					if (isAddedOrRemoved || isPossibleStructuralChange(delta.getFlags())) {
						return true;
					}
					return processChildrenDelta(delta);
				default:
					// fields, methods, imports ect
					return false;
			}	
		}
		
		private boolean isPossibleStructuralChange(int flags) {
			return (flags & (IJavaElementDelta.F_CONTENT | IJavaElementDelta.F_FINE_GRAINED)) == IJavaElementDelta.F_CONTENT;
		}		
		
		private boolean processChildrenDelta(IJavaElementDelta delta) {
			IJavaElementDelta[] children= delta.getAffectedChildren();
			for (int i= 0; i < children.length; i++) {
				if (processDelta(children[i])) {
					return true;
				}
			}
			return false;
		}
	}
	

}
