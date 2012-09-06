/******************************************************************************* 
 * Copyright (c) 2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.quickfix;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.jdt.ui.text.java.IJavaCompletionProposal;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;

public class QuickFixManager {
	private static QuickFixManager instance = null;
	
	private HashMap<String, IQuickFixGenerator> generators = new HashMap<String, IQuickFixGenerator>();
	
	public static QuickFixManager getInstance(){
		if(instance == null){
			instance = new QuickFixManager();
		}
		return instance;
	}
	
	public QuickFixManager(){
		QuickFixExtension[] extensions = QuickFixExtension.getInstances();
		for(QuickFixExtension extension : extensions){
			IQuickFixGenerator generator = extension.getQuickFixGenerator();
			if(generator != null){
				addQuickFixGenerator(generator);
			}
		}
	}
	
	public void addQuickFixGenerator(IQuickFixGenerator generator){
		if(!generators.containsKey(generator.getClass().toString())){
			generators.put(generator.getClass().toString(), generator);
		}
	}
	
	public void removeQuickFixGenerator(IQuickFixGenerator generator){
		generators.remove(generator);
	}
	
	public boolean hasProposals(Annotation annotation, Position position){
		for(IQuickFixGenerator generator : generators.values()){
			if(generator.hasProposals(annotation, position)){
				return true;
			}
		}
		return false;
	}
	
	public List<IJavaCompletionProposal> getProposals(Annotation annotation, Position position){
		ArrayList<IJavaCompletionProposal> proposals = new ArrayList<IJavaCompletionProposal>();
		for(IQuickFixGenerator generator : generators.values()){
			IJavaCompletionProposal[] pp = generator.getProposals(annotation, position);
			for(IJavaCompletionProposal p : pp){
				proposals.add(p);
			}
		}
		
		return proposals;
	}
}
