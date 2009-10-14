/*******************************************************************************
 * Copyright (c) 2009 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.el.ui.refactoring;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.internal.ui.search.JavaSearchResultPage;
import org.eclipse.jdt.ui.search.ElementQuerySpecification;
import org.eclipse.jdt.ui.search.IMatchPresentation;
import org.eclipse.jdt.ui.search.IQueryParticipant;
import org.eclipse.jdt.ui.search.ISearchRequestor;
import org.eclipse.jdt.ui.search.QuerySpecification;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.search.ui.text.Match;
import org.eclipse.ui.PartInitException;
import org.jboss.tools.common.el.core.refactoring.ELProjectSetExtension;
import org.jboss.tools.common.el.core.refactoring.ProjectsSet;
import org.jboss.tools.common.el.core.refactoring.RefactorSearcher;
import org.jboss.tools.common.model.project.ProjectHome;

public class ELReferencesQueryParticipant implements IQueryParticipant, IMatchPresentation{
	private ELSearcher searcher;
	JavaSearchResultPage searchPage = null;
	
	public int estimateTicks(QuerySpecification specification) {
		return 10;
	}

	public IMatchPresentation getUIParticipant() {
		return this;
	}

	public void search(ISearchRequestor requestor,
			QuerySpecification querySpecification, IProgressMonitor monitor)
			throws CoreException {
		
		if(querySpecification instanceof ElementQuerySpecification){
			ElementQuerySpecification qs = (ElementQuerySpecification)querySpecification;
			if(qs.getElement() instanceof IMethod){
				IFile file = (IFile)qs.getElement().getResource();
				String name = qs.getElement().getElementName();
				
				searcher = new ELSearcher(requestor, qs.getElement(), file, name);
				searcher.setSearchScope(qs.getScope());
				
				searcher.findELReferences();
			}
		}
	}
	
	public ILabelProvider createLabelProvider() {
		return null;
	}

	public void showMatch(Match match, int currentOffset,
			int currentLength, boolean activate) throws PartInitException {
	}
	
	class ELSearcher extends RefactorSearcher{
		ISearchRequestor requestor;
		ProjectsSet projectSet=null;
		
		public ELSearcher(ISearchRequestor requestor, IJavaElement element, IFile file, String name){
			super(file, name, element);
			this.requestor = requestor;
			ELProjectSetExtension[] extensions = 	ELProjectSetExtension.getInstances();
			if(extensions.length > 0){
				projectSet = extensions[0].getProjectSet();
				if(projectSet != null)
					projectSet.init(file.getProject());
			}
			
		}
		
		protected boolean isFileCorrect(IFile file){
			if(!file.isSynchronized(IResource.DEPTH_ZERO)){
				return false;
			}else if(file.isPhantom()){
				return false;
			}else if(file.isReadOnly()){
				return false;
			}
			return true;
		}

		@Override
		protected void match(IFile file, int offset, int length) {
			Match match = new Match(file, offset, length);
			requestor.reportMatch(match);
		}
		
		protected IProject[] getProjects(){
			if(projectSet != null){
				return projectSet.getLinkedProjects();
			}
			return new IProject[]{baseFile.getProject()};
		}
		
		protected IContainer getViewFolder(IProject project){
			if(projectSet != null){
				return projectSet.getViewFolder(project);
			}
			
			IPath path = ProjectHome.getFirstWebContentPath(baseFile.getProject()).removeFirstSegments(1);
			
			return baseFile.getProject().getFolder(path);
		}
	}
}
