/*******************************************************************************
 * Copyright (c) 2008 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.common.text.xml.contentassist;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.TextPresentation;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationExtension;
import org.eclipse.jface.text.contentassist.IContextInformationPresenter;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.graphics.Image;
import org.eclipse.wst.sse.core.internal.provisional.IModelManager;
import org.eclipse.wst.sse.core.internal.provisional.IStructuredModel;
import org.eclipse.wst.sse.core.internal.provisional.StructuredModelManager;
import org.eclipse.wst.sse.ui.internal.contentassist.IRelevanceCompletionProposal;
import org.eclipse.wst.sse.ui.internal.util.Sorter;

/**
 * Reads the plugin.xml file for the processors defined using the 
 * 	extention point 
 * 	"org.jboss.tools.common.text.xml.contentAssistProcessor"
 * 
 * @author jeremy
 *
 */

public class SortingCompoundContentAssistProcessor implements  IContentAssistProcessor {
	private ISourceViewer fSourceViewer;
	private String fPartitionType;
	private String fErrorMessage;
	
	
	private static Map<String, Map<String, List<IContentAssistProcessor>>> fProcessorsMap;
	
	public SortingCompoundContentAssistProcessor(ISourceViewer sourceViewer, String partitionType) {
		this.fSourceViewer = sourceViewer;
		this.fPartitionType = partitionType;
		init();
	}
	
	public boolean supportsPartitionType(String partitionType) {
		if (fProcessorsMap == null)
			return false;
		
		for (String contentType : fProcessorsMap.keySet()) {
			Map<String, List<IContentAssistProcessor>> partitionTypes = 
				fProcessorsMap.get(contentType);
			if (partitionTypes != null && partitionTypes.containsKey(partitionType))
				return true;
		}
		return false;
	}
	
	void init () {
		ContentAssistProcessorDefinition[] defs = ContentAssistProcessorBuilder.getInstance().getContentAssistProcessorDefinitions(fPartitionType);

		if(defs==null || defs.length == 0) return;

		if (fProcessorsMap == null)
			fProcessorsMap = new HashMap<String, Map<String, List<IContentAssistProcessor>>>();
		
		for(int i=0; i<defs.length; i++) {
		    IContentAssistProcessor processor = defs[i].createContentAssistProcessor();
		    Collection<String> contentTypes = defs[i].getContentTypes();
		    if (contentTypes != null) {
		    	for (String contentType : contentTypes) {
		    		Map<String, List<IContentAssistProcessor>> contentTypeProcessors = 
		    			fProcessorsMap.get(contentType);
		    		
		    		if (contentTypeProcessors == null) {
		    			contentTypeProcessors = new HashMap<String, List<IContentAssistProcessor>>();
		    		}
		    		
		    		List<String> partitionTypes = defs[i].getPartitionTypes(contentType);
		    		for (String partitionType : partitionTypes) {
		    			List<IContentAssistProcessor> partitionTypeProcessors =
		    				contentTypeProcessors.get(partitionType);
		    			
		    			if (partitionTypeProcessors == null) {
		    				partitionTypeProcessors = new ArrayList<IContentAssistProcessor>();
		    			}
		    			
		    			if(!containsAnObjectOfTheSameType(partitionTypeProcessors,processor)) {
		    				partitionTypeProcessors.add(processor);
		    			}

		    			if (!contentTypeProcessors.containsKey(partitionType)) {
		    				contentTypeProcessors.put(partitionType, partitionTypeProcessors);
		    			}
		    		}
		    		
		    		if (!fProcessorsMap.containsKey(contentType)) {
		    			fProcessorsMap.put(contentType, contentTypeProcessors);
		    		}
		    	}
		    }
		}

	}

	private static final String POST_INSTALL_CA_PROCESSORS_CONTENTTYPE_STUB = "$$POST_INSTALL_CA_PROCESSORS_CONTENTTYPE_STUB$$"; //$NON-NLS-1$
	public void addContentAssistProcessor(String partitionType, IContentAssistProcessor processor) {
		if (partitionType == null || processor == null)
			return;

		Map<String, List<IContentAssistProcessor>> contentTypeProcessors = fProcessorsMap.get(POST_INSTALL_CA_PROCESSORS_CONTENTTYPE_STUB);
		if (contentTypeProcessors == null) {
			contentTypeProcessors = new HashMap<String, List<IContentAssistProcessor>>();
		}
		
		List<IContentAssistProcessor> partitionTypeProcessors = contentTypeProcessors.get(partitionType);
		if (partitionTypeProcessors == null) {
			partitionTypeProcessors = new ArrayList<IContentAssistProcessor>();
		}

		if(!containsAnObjectOfTheSameType(partitionTypeProcessors,processor)) {
			partitionTypeProcessors.add(processor);
		}

		if (!contentTypeProcessors.containsKey(partitionType)) {
			contentTypeProcessors.put(partitionType, partitionTypeProcessors);
		}

		if (!fProcessorsMap.containsKey(POST_INSTALL_CA_PROCESSORS_CONTENTTYPE_STUB)) {
			fProcessorsMap.put(POST_INSTALL_CA_PROCESSORS_CONTENTTYPE_STUB, contentTypeProcessors);
		}
	}
	
	boolean containsAnObjectOfTheSameType(Collection collection, Object obj) {
		if (collection == null || obj == null)
			return false;
		
		String objClassName = obj.getClass().getName();
		if (objClassName == null)
			return false;
		
		for (Object o : collection) {
			if (o == null || o.getClass() == null) 
				continue;
			
			if (objClassName.equals(o.getClass().getName()))
				return true;
		}
		return false;
	}
	
	/**
	 * Returns the content type of document
	 * 
	 * @param document -
	 *            assumes document is not null
	 * @return String content type of given document
	 */
	protected String getContentType(ISourceViewer viewer) {
		if (viewer == null || viewer.getDocument() == null)
			return null;
		
		String type = null;
		
		IModelManager mgr = StructuredModelManager.getModelManager();
		IStructuredModel model = null;
		try {
			model = mgr.getExistingModelForRead(viewer.getDocument());
			if (model != null) {
				type = model.getContentTypeIdentifier();
			}
		} finally {
			if (model != null) {
				model.releaseFromRead();
			}
		}
		return type;
	}

	/**
	 * Returns a list of completion proposals based on the
	 * specified location within the document that corresponds
	 * to the current cursor position within the text viewer.
	 *
	 * @param viewer the viewer whose document is used to compute the proposals
	 * @param offset an offset within the document for which completions should be computed
	 * @return an array of completion proposals or <code>null</code> if no proposals are possible
	 */
	public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer, int offset) {
		fErrorMessage = null;

		String contentType = getContentType(fSourceViewer);
		if (contentType == null)
			return new ICompletionProposal[0];
		
		List<ICompletionProposal> ret = new LinkedList<ICompletionProposal>();

		List<IContentAssistProcessor> processors = new LinkedList<IContentAssistProcessor>();
		if (fProcessorsMap.get(contentType) != null) {
			List<IContentAssistProcessor> list = fProcessorsMap.get(contentType).get(fPartitionType);
			if (list != null) {
				processors.addAll(list);
			}
		}
		if (fProcessorsMap.get(POST_INSTALL_CA_PROCESSORS_CONTENTTYPE_STUB) != null) {
			List<IContentAssistProcessor> list = fProcessorsMap.get(POST_INSTALL_CA_PROCESSORS_CONTENTTYPE_STUB).get(fPartitionType);
			if(list != null) {
				processors.addAll(list);
			}
		}
		if(processors.size() == 0) {
			return new ICompletionProposal[0];
		}
		
		
		for (IContentAssistProcessor p : processors) {
			ICompletionProposal[] proposals = p.computeCompletionProposals(viewer, offset);
			if (proposals != null && proposals.length > 0) {
				ret.addAll(Arrays.asList(proposals));
				fErrorMessage = null; // Hide previous errors
			} else {
				if (fErrorMessage == null && ret.isEmpty()) {
					String errorMessage = p.getErrorMessage();
					if (errorMessage != null) {
						fErrorMessage = errorMessage;
					}
				}
			}
		}
		
		ICompletionProposal[] resultArray = ret.toArray(new ICompletionProposal[ret.size()]);
		Object[] sorted = createSorter().sort(resultArray);
		System.arraycopy(sorted, 0, resultArray, 0, sorted.length);
		resultArray = makeUnique(resultArray);
		
		return resultArray;
	}

	/**
	 * Removes duplicates of completion strings
	 *
	 * @param suggestions a list of suggestions ({@link String}).
	 * @return a list of unique completion suggestions.
	 */
	public ICompletionProposal[] makeUnique(ICompletionProposal[] proposals) {
		HashSet<String> present = new HashSet<String>();
		HashSet<String> info = new HashSet<String>();
		ArrayList<ICompletionProposal> unique= new ArrayList<ICompletionProposal>();

		for (int i = 0; proposals != null && i < proposals.length; i++) {
			String infoUnquoted = proposals[i].getAdditionalProposalInfo();
			if (infoUnquoted != null) {
				if (infoUnquoted.startsWith("\""))
					infoUnquoted = infoUnquoted.substring(1);
				if (infoUnquoted.endsWith("\""))
					infoUnquoted = infoUnquoted.substring(0, infoUnquoted.length() - 1);
				infoUnquoted = infoUnquoted.trim().toLowerCase();
			}
			String displayStringUnquoted = proposals[i].getDisplayString().toLowerCase();
			if (displayStringUnquoted.startsWith("\""))
				displayStringUnquoted = displayStringUnquoted.substring(1);
			if (displayStringUnquoted.endsWith("\""))
				displayStringUnquoted = displayStringUnquoted.substring(0, displayStringUnquoted.length() - 1);
			displayStringUnquoted = displayStringUnquoted.trim().toLowerCase();
			
			if (!present.contains(displayStringUnquoted)) {
				present.add(displayStringUnquoted);
				if (infoUnquoted != null && infoUnquoted.length() > 0) {
					if (!info.contains(infoUnquoted)) {
						info.add(infoUnquoted);
					} else {
						// Do not add proposals with the same info
						continue;
					}
				}
				unique.add(proposals[i]);
			}
		}

		present.clear();
		return unique.toArray(new ICompletionProposal[unique.size()]);
	}

	protected Sorter createSorter() {
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
	 * Returns information about possible contexts based on the
	 * specified location within the document that corresponds
	 * to the current cursor position within the text viewer.
	 *
	 * @param viewer the viewer whose document is used to compute the possible contexts
	 * @param offset an offset within the document for which context information should be computed
	 * @return an array of context information objects or <code>null</code> if no context could be found
	 */
	public IContextInformation[] computeContextInformation(ITextViewer viewer, int offset) {
		fErrorMessage = null;
		
		String contentType = getContentType(fSourceViewer);
		if (contentType == null)
			return new IContextInformation[0];
		
		List<IContextInformation> ret = new LinkedList<IContextInformation>();

		if (fProcessorsMap.get(contentType) == null)
			return new IContextInformation[0];

		if (fProcessorsMap.get(contentType).get(fPartitionType) == null)
			return new IContextInformation[0];
		
		for (IContentAssistProcessor p : fProcessorsMap.get(contentType).get(fPartitionType)) {
			IContextInformation[] informations = p.computeContextInformation(viewer, offset);
			if (informations != null && informations.length > 0) {
				for (int i = 0; i < informations.length; i++)
					ret.add(new ContextInformation(informations[i], p));
				fErrorMessage = null; // Hide previous errors
			} else {
				if (fErrorMessage == null && ret.isEmpty()) {
					String errorMessage = p.getErrorMessage();
					if (errorMessage != null) {
						fErrorMessage = errorMessage;
					}
				}
			}
		}
		return (IContextInformation[]) ret.toArray(new IContextInformation[ret.size()]);
	}

	/**
	 * Returns the characters which when entered by the user should
	 * automatically trigger the presentation of possible completions.
	 *
	 * @return the auto activation characters for completion proposal or <code>null</code>
	 *		if no auto activation is desired
	 */
	public char[] getCompletionProposalAutoActivationCharacters() {
		String contentType = getContentType(fSourceViewer);
		if (contentType == null)
			return new char[0];
		
		List<Character> ret = new LinkedList<Character>();

		if (fProcessorsMap.get(contentType) == null)
			return new char[0];

		if (fProcessorsMap.get(contentType).get(fPartitionType) == null)
			return new char[0];
		
		for (IContentAssistProcessor p : fProcessorsMap.get(contentType).get(fPartitionType)) {
			char[] chars = p.getCompletionProposalAutoActivationCharacters();
			if (chars != null)
				for (int i = 0; i < chars.length; i++)
					ret.add(new Character(chars[i]));
		}

		char[] chars = new char[ret.size()];
		int i = 0;
		for (Iterator it = ret.iterator(); it.hasNext(); i++) {
			Character ch = (Character) it.next();
			chars[i] = ch.charValue();
		}
		return chars;
	}

	/**
	 * Returns the characters which when entered by the user should
	 * automatically trigger the presentation of context information.
	 *
	 * @return the auto activation characters for presenting context information
	 *		or <code>null</code> if no auto activation is desired
	 */
	public char[] getContextInformationAutoActivationCharacters() {
		String contentType = getContentType(fSourceViewer);
		if (contentType == null)
			return new char[0];
		
		List<Character> ret = new LinkedList<Character>();

		if (fProcessorsMap.get(contentType) == null)
			return new char[0];

		if (fProcessorsMap.get(contentType).get(fPartitionType) == null)
			return new char[0];
		
		for (IContentAssistProcessor p : fProcessorsMap.get(contentType).get(fPartitionType)) {
			char[] chars = p.getContextInformationAutoActivationCharacters();
			if (chars != null)
				for (int i = 0; i < chars.length; i++)
					ret.add(new Character(chars[i]));
		}

		char[] chars = new char[ret.size()];
		int i = 0;
		for (Iterator it = ret.iterator(); it.hasNext(); i++) {
			Character ch = (Character) it.next();
			chars[i] = ch.charValue();
		}
		return chars;
	}

	/**
	 * Returns the reason why this content assist processor
	 * was unable to produce any completion proposals or context information.
	 *
	 * @return an error message or <code>null</code> if no error occurred
	 */
	public String getErrorMessage() {
		return fErrorMessage;
	}

	/**
	 * Returns a validator used to determine when displayed context information
	 * should be dismissed. May only return <code>null</code> if the processor is
	 * incapable of computing context information. <p>
	 *
	 * @return a context information validator, or <code>null</code> if the processor
	 * 			is incapable of computing context information
	 */
	public IContextInformationValidator getContextInformationValidator() {
		boolean hasValidator = false;
		boolean hasPresenter = false;
		boolean hasExtension = false;


		String contentType = getContentType(fSourceViewer);
		if (contentType == null)
			return null;
		
		List<Character> ret = new LinkedList<Character>();

		if (fProcessorsMap.get(contentType) == null)
			return null;

		if (fProcessorsMap.get(contentType).get(fPartitionType) == null)
			return null;
		
		for (IContentAssistProcessor p : fProcessorsMap.get(contentType).get(fPartitionType)) {
			IContextInformationValidator v = p.getContextInformationValidator();
			if (v != null) {
				hasValidator = true;
				if (v instanceof IContextInformationPresenter) {
					hasPresenter = true;
				}
			}
		}

		SortingCompoundContentAssistValidator validator = null;
		if (hasPresenter)
			validator = new SortingCompoundContentAssistValidatorPresenter();
		else if (hasValidator)
			validator = new SortingCompoundContentAssistValidator();

		if (validator != null)
			for (IContentAssistProcessor p : fProcessorsMap.get(contentType).get(fPartitionType)) {
				IContextInformationValidator v = p.getContextInformationValidator();
				if (v != null)
					validator.add(v);
			}

		return validator;
	}

	
	static class ContextInformation implements IContextInformation, IContextInformationExtension {
		private IContextInformation fInfo;
		private IContentAssistProcessor fProcessor;

		ContextInformation(IContextInformation info, IContentAssistProcessor processor) {
			fInfo = info;
			fProcessor = processor;
		}

		/*
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		public boolean equals(Object obj) {
			return fInfo.equals(obj);
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.IContextInformation#getContextDisplayString()
		 */
		public String getContextDisplayString() {
			return fInfo.getContextDisplayString();
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.IContextInformation#getImage()
		 */
		public Image getImage() {
			return fInfo.getImage();
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.IContextInformation#getInformationDisplayString()
		 */
		public String getInformationDisplayString() {
			return fInfo.getInformationDisplayString();
		}

		/*
		 * @see java.lang.Object#hashCode()
		 */
		public int hashCode() {
			return fInfo.hashCode();
		}

		/*
		 * @see java.lang.Object#toString()
		 */
		public String toString() {
			return fInfo.toString();
		}

		IContentAssistProcessor getProcessor() {
			return fProcessor;
		}

		IContextInformation getContextInformation() {
			return fInfo;
		}

		public int getContextInformationPosition() {
			int position = -1;
			if (fInfo instanceof IContextInformationExtension)
				position = ((IContextInformationExtension)fInfo).getContextInformationPosition();
			return position;
		}
	}

	private static class SortingCompoundContentAssistValidator implements IContextInformationValidator {
		List fValidators = new ArrayList();
		IContextInformationValidator fValidator;

		void add(IContextInformationValidator validator) {
			fValidators.add(validator);
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.IContextInformationValidator#install(org.eclipse.jface.text.contentassist.IContextInformation,
		 *      org.eclipse.jface.text.ITextViewer, int)
		 */
		public void install(IContextInformation info, ITextViewer viewer, int documentPosition) {
			// install either the validator in the info, or all validators
			fValidator = getValidator(info);
			IContextInformation realInfo = getContextInformation(info);
			if (fValidator != null)
				fValidator.install(realInfo, viewer, documentPosition);
			else {
				for (Iterator it = fValidators.iterator(); it.hasNext();) {
					IContextInformationValidator v = (IContextInformationValidator) it.next();
					v.install(realInfo, viewer, documentPosition);
				}
			}
		}

		IContextInformationValidator getValidator(IContextInformation info) {
			if (info instanceof ContextInformation) {
				ContextInformation wrap = (ContextInformation) info;
				return wrap.getProcessor().getContextInformationValidator();
			}

			return null;
		}

		IContextInformation getContextInformation(IContextInformation info) {
			IContextInformation realInfo = info;
			if (info instanceof ContextInformation) {
				ContextInformation wrap = (ContextInformation) info;
				realInfo = wrap.getContextInformation();
			}

			return realInfo;
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.IContextInformationValidator#isContextInformationValid(int)
		 */
		public boolean isContextInformationValid(int documentPosition) {
			// use either the validator in the info, or all validators
			boolean isValid = false;
			if (fValidator != null)
				isValid = fValidator.isContextInformationValid(documentPosition);
			else {
				for (Iterator it = fValidators.iterator(); it.hasNext();) {
					IContextInformationValidator v = (IContextInformationValidator) it.next();
					isValid |= v.isContextInformationValid(documentPosition);
				}
			}
			return isValid;
		}

	}

	private static class SortingCompoundContentAssistValidatorPresenter extends SortingCompoundContentAssistValidator implements IContextInformationPresenter {
		public boolean updatePresentation(int offset, TextPresentation presentation) {
			// use either the validator in the info, or all validators
			boolean presentationUpdated = false;
			if (fValidator instanceof IContextInformationPresenter)
				presentationUpdated = ((IContextInformationPresenter) fValidator).updatePresentation(offset, presentation);
			else {
				for (Iterator it = fValidators.iterator(); it.hasNext();) {
					IContextInformationValidator v = (IContextInformationValidator) it.next();
					if (v instanceof IContextInformationPresenter)
						presentationUpdated |= ((IContextInformationPresenter) v).updatePresentation(offset, presentation);
				}
			}
			return presentationUpdated;
		}
	}
}
