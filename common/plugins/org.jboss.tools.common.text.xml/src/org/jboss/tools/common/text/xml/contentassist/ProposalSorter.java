/*******************************************************************************
 * Copyright (c) 2010-2014 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.text.xml.contentassist;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.wst.sse.ui.contentassist.CompletionProposalInvocationContext;
import org.eclipse.wst.sse.ui.internal.contentassist.CustomCompletionProposal;
import org.eclipse.wst.sse.ui.internal.contentassist.IRelevanceCompletionProposal;
import org.eclipse.wst.sse.ui.internal.util.Sorter;
import org.jboss.tools.common.text.ITextProposalProvider;
import org.jboss.tools.common.text.TextProposal;

/**
 * The utility helper class which purpose is to filter out and sort 
 * Content Assist proposals
 * 
 * @author Victor V. Rubezhny
 */
@SuppressWarnings("restriction")
public class ProposalSorter {
	
	/**
	 * Utility that helps to ProposalSorter object to filter out the proposals that aren't valid for the 
	 * given context
	 */
	public static interface IProposalFilter {
		boolean isValidProposal(CompletionProposalInvocationContext context, ICompletionProposal proposal);
	}
	
	/**
	 * Filters and sorts the Content Assist proposals, using the external filter
	 */
	public static List<ICompletionProposal> filterAndSortProposals(List<ICompletionProposal> proposals,
			IProgressMonitor monitor, CompletionProposalInvocationContext context, IProposalFilter filter) {
		if (proposals == null)
			return null;
		ICompletionProposal[] resultArray = (ICompletionProposal[])proposals.toArray(new ICompletionProposal[proposals.size()]);
		resultArray = makeUnique(resultArray);
		resultArray = filterProposals(resultArray, context, filter);
		Object[] sorted = createSorter().sort(resultArray);
		System.arraycopy(sorted, 0, resultArray, 0, sorted.length);
		return Arrays.asList(resultArray);
	}
	
	/**
	 * Filters and sorts the Content Assist proposals
	 */
	public static List<ICompletionProposal> filterAndSortProposals(List<ICompletionProposal> proposals,
			IProgressMonitor monitor, CompletionProposalInvocationContext context) {
		return filterAndSortProposals(proposals, monitor, context, null);
	}
	
	private static Sorter createSorter() {
		return new Sorter() {
			public boolean compare(Object proposal1, Object proposal2) {

				int pr1 = Integer.MIN_VALUE;
				int pr2 = Integer.MIN_VALUE;
				
				ICompletionProposal p1 = (ICompletionProposal)proposal1;
				ICompletionProposal p2 = (ICompletionProposal)proposal2;
				
				if (p1 instanceof IRelevanceCompletionProposal)
					pr1 = ((IRelevanceCompletionProposal)p1).getRelevance();

				if (p2 instanceof IRelevanceCompletionProposal)
					pr2 = ((IRelevanceCompletionProposal)p2).getRelevance();
				
				
				if (pr1 == pr2) {
					String str1 = (p1.getDisplayString() == null ? "" : p1.getDisplayString()); //$NON-NLS-1$
					String str2 = (p2.getDisplayString() == null ? "" : p2.getDisplayString()); //$NON-NLS-1$
					return str2.compareTo(str1) > 0;
				}

				return (pr1 > pr2);
			}
		};
	}

	/**
	 * Removes duplicates of completion strings
	 *
	 * @param suggestions a list of suggestions ({@link String}).
	 * @return a list of unique completion suggestions.
	 */
	public static ICompletionProposal[] makeUnique(ICompletionProposal[] proposals) {
		if (proposals == null)
			return null;
		
		Map <String, ICompletionProposal> existingProposals = new HashMap<String, ICompletionProposal>(proposals.length);
		ArrayList<ICompletionProposal> unique = new ArrayList<ICompletionProposal>(proposals.length);
		
		for (ICompletionProposal proposal : proposals) {
			if (proposal == null)
				continue;

			String replString = null;
			String dispString = null;

			if (proposal instanceof CustomCompletionProposal) {
				replString = ((CustomCompletionProposal) proposal)
						.getReplacementString();
			}
			dispString = unQuote(proposal.getDisplayString());
			replString = getReplacementWord(replString == null ? dispString
					: replString);
			
			boolean isFilterable = true;
			if (proposal instanceof ITextProposalProvider) { 
				TextProposal textProposal = ((ITextProposalProvider)proposal).getTextProposal();
				isFilterable = (textProposal == null || textProposal.isFilterable());
			}

			if (isFilterable) {
				ICompletionProposal existingProposal = existingProposals.get(replString);
				if (existingProposal == null) {
					existingProposals.put(replString, proposal);
					unique.add(proposal);
				} else {
					if (existingProposal instanceof IRelevanceCompletionProposal && proposal instanceof IRelevanceCompletionProposal) {
						if (((IRelevanceCompletionProposal)existingProposal).getRelevance() <
								((IRelevanceCompletionProposal)proposal).getRelevance()) {
							existingProposals.put(replString, proposal);
							unique.remove(existingProposal);
							unique.add(proposal);
						}
					}
				}
			} else {
				unique.add(proposal);
			}
		}
		return unique.toArray(new ICompletionProposal[unique.size()]);
	}

	public static ICompletionProposal[] filterProposals(ICompletionProposal[] proposals,
			CompletionProposalInvocationContext context, IProposalFilter filter) {
		if (filter == null)
			return proposals;

		ArrayList<ICompletionProposal> filtered = new ArrayList<ICompletionProposal>(proposals.length);
		for (ICompletionProposal proposal : proposals) {
			if (filter.isValidProposal(context, proposal)) {
				filtered.add(proposal);
			}
		}
		return filtered.toArray(new ICompletionProposal[filtered.size()]);
	}
	
	private static String getReplacementWord(String replacement) {
		replacement = (replacement == null ? "" : //$NON-NLS-1$
				replacement);
		int index = replacement.indexOf('>');
		if (index != -1) {
			replacement = replacement.substring(0, index).trim();
			if (replacement.endsWith("/")) //$NON-NLS-1$
				replacement = replacement
						.substring(0, replacement.length() - 1).trim();
		}
		index = replacement.indexOf("="); //$NON-NLS-1$
		if (index != -1) {
			replacement = replacement.substring(0, index).trim();
		}
		index = replacement.indexOf(" "); //$NON-NLS-1$
		if (index != -1) {
			replacement = replacement.substring(0, index).trim();
		}
		return replacement;
	}

	private static String unQuote(String str) {
		str = (str == null ?
				"" :  //$NON-NLS-1$
				str.toLowerCase());
		if (str.startsWith("\"")) //$NON-NLS-1$
			str = str.substring(1);
		if (str.endsWith("\"")) //$NON-NLS-1$
			str = str.substring(0, str.length() - 1);
		str = str.trim().toLowerCase();
		
		return str;
	}
}
