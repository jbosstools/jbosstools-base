/*******************************************************************************
  * Copyright (c) 2011 - 2012 Red Hat, Inc.
  * Distributed under license by Red Hat, Inc. All rights reserved.
  * This program is made available under the terms of the
  * Eclipse Public License v1.0 which accompanies this distribution,
  * and is available at http://www.eclipse.org/legal/epl-v10.html
  *
  * Contributor:
  *     Red Hat, Inc. - initial API and implementation
  ******************************************************************************/
package org.jboss.tools.common.base.test.kb;

import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.ILocalVariable;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.core.search.IJavaSearchScope;
import org.eclipse.jdt.internal.ui.search.JavaSearchScopeFactory;
import org.eclipse.jdt.ui.search.ElementQuerySpecification;
import org.eclipse.jdt.ui.search.IQueryParticipant;
import org.eclipse.jdt.ui.search.ISearchRequestor;
import org.eclipse.jdt.ui.search.QuerySpecification;
import org.eclipse.search.ui.text.Match;
import org.jboss.tools.common.EclipseUtil;
import org.jboss.tools.common.util.FileUtil;

public class QueryParticipantTestUtils extends TestCase {
	public static final int FIELD_SEARCH = 1;
	public static final int METHOD_SEARCH = 2;
	public static final int TYPE_SEARCH = 3;
	public static final int PARAMETER_SEARCH = 4;

	public static void testSearchParticipant(IProject project, String fileName, int searchType, String elementName, String parameterName, IQueryParticipant participant, List<MatchStructure> matches) throws CoreException{
		IFile file = project.getFile(fileName);
		assertNotNull("File - " + fileName + " not found", file);

		ICompilationUnit compilationUnit = EclipseUtil.getCompilationUnit(file);

		assertNotNull("CompilationUnit not found", compilationUnit);

		IJavaElement element = null;

		IType type = compilationUnit.findPrimaryType();

		assertNotNull("Primary type not found", type);

		if(searchType == FIELD_SEARCH){
			element = type.getField(elementName);
		} else if(searchType == METHOD_SEARCH){
			element = getMethod(type, elementName);
		} else if(searchType == TYPE_SEARCH){
			element = type;
		} else if(searchType == PARAMETER_SEARCH){
			IMethod method = getMethod(type, elementName);
			element = getParameter(method, parameterName);
		}

		assertNotNull("Java Element not found", element);

		SearchRequestor requestor = new SearchRequestor();

		JavaSearchScopeFactory factory = JavaSearchScopeFactory.getInstance();
		IJavaSearchScope scope = factory.createWorkspaceScope(true);
		String description = factory.getWorkspaceScopeDescription(true);
		QuerySpecification specification = new ElementQuerySpecification(element, IJavaSearchConstants.REFERENCES, scope, description);

		participant.search(requestor, specification, new NullProgressMonitor());

		List<Match> matchesForCheck = requestor.getMatches();

		checkMatches(matchesForCheck, matches);
	}

	private static IMethod getMethod(IType type, String name) throws JavaModelException{
		IMethod[] methods = type.getMethods();
		for(IMethod method : methods){
			if(method.getElementName().equals(name))
				return method;
		}
		return null;
	}

	private static ILocalVariable getParameter(IMethod method, String name) throws JavaModelException{
		ILocalVariable[] parameters = method.getParameters();
		for(ILocalVariable parameter : parameters) {
			if(parameter.getElementName().equals(name)) {
				return parameter;
			}
		}
		return null;
	}

	private static void checkMatches(List<Match> matchesForCheck, List<MatchStructure> matchList) throws CoreException {
		for(Match match : matchesForCheck){
			assertTrue("Match must return IFile", match.getElement() instanceof IFile);

			IFile file = (IFile)match.getElement();
			String filePath = file.getFullPath().toString();
			String text = FileUtil.getContentFromEditorOrFile(file);
			String name = text.substring(match.getOffset(), match.getOffset()+match.getLength());

			MatchStructure ms = findMatch(matchList, match, filePath, name);

			assertNotNull("Unexpected match found (file - " + filePath + " name - " + name + ")", ms);
			ms.checked = true;
		}

		for(MatchStructure ms : matchList){
			assertTrue("Match not found (file - "+ms.path+" name - "+ms.name+")", ms.checked);
		}
	}

	protected static MatchStructure findMatch(List<MatchStructure> matchList, Match match, String filePath, String name){
		for(MatchStructure ms : matchList){
			if(!ms.checked && ms.path.equals(filePath) && ms.name.equals(name)){
				//System.out.println("Match found (file - "+ms.path+" name - "+ms.name+")");
				return ms;
			}
		}
		return null;
	}

	static class SearchRequestor implements ISearchRequestor {
		ArrayList<Match> matches = new ArrayList<Match>();

		public void reportMatch(Match match){
			matches.add(match);
		}

		public List<Match> getMatches(){
			return matches;
		}
	}

	public static class MatchStructure{
		String path;
		String name; // label
		boolean checked;

		public MatchStructure(String path, String name){
			this.path = path;
			this.name = name;
			checked = false;
		}
	}
}