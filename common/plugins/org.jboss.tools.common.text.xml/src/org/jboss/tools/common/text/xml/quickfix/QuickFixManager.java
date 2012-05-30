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
package org.jboss.tools.common.text.xml.quickfix;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.jface.text.contentassist.ICompletionProposal;
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
	
	public boolean hasProposals(Annotation annotation){
		for(IQuickFixGenerator generator : generators.values()){
			if(generator.hasProposals(annotation)){
				return true;
			}
		}
		return false;
	}
	
	public List<ICompletionProposal> getProposals(Annotation annotation){
		ArrayList<ICompletionProposal> proposals = new ArrayList<ICompletionProposal>();
		for(IQuickFixGenerator generator : generators.values()){
			List<ICompletionProposal> pp = generator.getProposals(annotation);
			proposals.addAll(pp);
		}
		
		return proposals;
	}
}
