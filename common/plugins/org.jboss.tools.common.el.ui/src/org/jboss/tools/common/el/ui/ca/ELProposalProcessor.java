/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.common.el.ui.ca;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.jdt.ui.PreferenceConstants;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposalExtension;
import org.eclipse.jface.text.contentassist.ICompletionProposalExtension2;
import org.eclipse.jface.text.contentassist.ICompletionProposalExtension3;
import org.eclipse.jface.text.contentassist.ICompletionProposalExtension4;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.wst.sse.core.internal.provisional.IStructuredModel;
import org.eclipse.wst.sse.core.internal.provisional.IndexedRegion;
import org.eclipse.wst.sse.core.internal.provisional.StructuredModelManager;
import org.eclipse.wst.sse.core.internal.provisional.text.IStructuredDocument;
import org.eclipse.wst.sse.core.internal.provisional.text.IStructuredDocumentRegion;
import org.eclipse.wst.sse.core.internal.provisional.text.ITextRegion;
import org.eclipse.wst.sse.ui.internal.contentassist.ContentAssistUtils;
import org.eclipse.wst.sse.ui.internal.contentassist.IRelevanceCompletionProposal;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMModel;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMNode;
import org.eclipse.wst.xml.core.internal.regions.DOMRegionContext;
import org.eclipse.wst.xml.ui.internal.contentassist.AbstractContentAssistProcessor;
import org.eclipse.wst.xml.ui.internal.util.SharedXMLEditorPluginImageHelper;
import org.jboss.tools.common.el.core.ca.AbstractELCompletionEngine;
import org.jboss.tools.common.el.core.model.ELInvocationExpression;
import org.jboss.tools.common.el.core.model.ELModel;
import org.jboss.tools.common.el.core.model.ELUtil;
import org.jboss.tools.common.el.core.resolver.ELResolver;
import org.jboss.tools.common.el.core.resolver.ELResolverFactoryManager;
import org.jboss.tools.common.el.core.resolver.ElVarSearcher;
import org.jboss.tools.common.el.core.resolver.SimpleELContext;
import org.jboss.tools.common.el.core.resolver.Var;
import org.jboss.tools.common.el.ui.ElUiPlugin;
import org.jboss.tools.common.model.ui.texteditors.xmleditor.XMLTextEditor;
import org.jboss.tools.common.text.TextProposal;
import org.jboss.tools.common.text.ext.IEditorWrapper;
import org.jboss.tools.common.text.ext.util.Utils;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

/**
 * Content assist proposal processor.
 * Computes Seam EL proposals.
 * 
 * @author Jeremy
 */
public abstract class ELProposalProcessor extends AbstractContentAssistProcessor {

	private static final ICompletionProposal[] NO_PROPOSALS= new ICompletionProposal[0];
	private static final IContextInformation[] NO_CONTEXTS= new IContextInformation[0];

	public static final class Proposal implements ICompletionProposal, ICompletionProposalExtension, ICompletionProposalExtension2, ICompletionProposalExtension3, ICompletionProposalExtension4, IRelevanceCompletionProposal {

		private final String fString;
		private final String fPrefix;
		private final String fNewPrefix;
		private final int fOffset;
		private int fNewPosition;

		private Image fImage;

		public Proposal(String string, String prefix, int offset) {
			this(string, prefix, offset, offset + string.length());
		}

		public Proposal(String string, String prefix, int offset, int newPosition) {
			this(string, prefix, prefix, offset, offset + string.length(), null);
		}

		public Proposal(String string, String prefix, int offset, int newPosition, Image image) {
			this(string, prefix, prefix, offset, offset + string.length(), image);
		}

		public Proposal(String string, String prefix, String newPrefix, int offset, int newPosition) {
			this(string, prefix, newPrefix, offset, newPosition, null);
		}

		public Proposal(String string, String prefix, String newPrefix, int offset, int newPosition, Image image) {
			fString = string;
			fPrefix = prefix;
			fNewPrefix = newPrefix;
			fOffset = offset;
			fNewPosition = newPosition;
			fImage = image;
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.ICompletionProposal#apply(IDocument)
		 */
		public void apply(IDocument document) {
			apply(null, '\0', 0, fOffset);
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.ICompletionProposal#getSelection(IDocument)
		 */
		public Point getSelection(IDocument document) {
			return new Point(fNewPosition, 0);
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.ICompletionProposal#getAdditionalProposalInfo()
		 */
		public String getAdditionalProposalInfo() {
			return null;
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.ICompletionProposal#getDisplayString()
		 */
		public String getDisplayString() {
			String dispString = (fNewPrefix == null ? fPrefix : fNewPrefix) + fString;
			if (dispString != null) {
				if (dispString.indexOf('{') == -1 
					&& dispString.indexOf('}') != -1) {
					dispString = dispString.substring(0, dispString.indexOf('}'));
				}
				if (dispString.indexOf('{') != -1 
					&& dispString.indexOf('}') == -1) {
					dispString += "}"; //$NON-NLS-1$
				}
			}
			return dispString;
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.ICompletionProposal#getImage()
		 */
		public Image getImage() {
			return fImage == null ? 
					SharedXMLEditorPluginImageHelper.getImage(SharedXMLEditorPluginImageHelper.IMG_OBJ_ATTRIBUTE) :
					fImage;
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.ICompletionProposal#getContextInformation()
		 */
		public IContextInformation getContextInformation() {
			return null;
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.ICompletionProposalExtension#apply(IDocument, char, int)
		 */
		public void apply(IDocument document, char trigger, int offset) {
			try {
				int docCharsToReplace = (fNewPrefix == null || fPrefix == null) ? 0 :
					fPrefix.length() - fNewPrefix.length();
				String replacement= fString.substring(offset - fOffset);
				document.replace(offset - docCharsToReplace, docCharsToReplace, replacement);
			} catch (BadLocationException x) {
				ElUiPlugin.getDefault().logError(x);
			}
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.ICompletionProposalExtension#isValidFor(IDocument, int)
		 */
		public boolean isValidFor(IDocument document, int offset) {
			return validate(document, offset, null);
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.ICompletionProposalExtension#getTriggerCharacters()
		 */
		public char[] getTriggerCharacters() {
			return null;
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.ICompletionProposalExtension#getContextInformationPosition()
		 */
		public int getContextInformationPosition() {
			return 0;
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.ICompletionProposalExtension2#apply(ITextViewer, char, int, int)
		 */
		public void apply(ITextViewer viewer, char trigger, int stateMask, int offset) {
			apply(viewer.getDocument(), trigger, offset);
			
			if (fString != null && fString.endsWith("}")) { //$NON-NLS-1$
				fNewPosition -= 1;
			}
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.ICompletionProposalExtension2#selected(ITextViewer, boolean)
		 */
		public void selected(ITextViewer viewer, boolean smartToggle) {
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.ICompletionProposalExtension2#unselected(ITextViewer)
		 */
		public void unselected(ITextViewer viewer) {
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.ICompletionProposalExtension2#validate(IDocument document, int offset, DocumentEvent event)
		 */
		public boolean validate(IDocument document, int offset, DocumentEvent event) {
			try {
				int prefixStart= fOffset - fPrefix.length();
				return offset >= fOffset && offset < fOffset + fString.length() && document.get(prefixStart, offset - (prefixStart)).equals((fPrefix + fString).substring(0, offset - prefixStart));
			} catch (BadLocationException x) {
				return false;
			} 
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.ICompletionProposalExtension3#getInformationControlCreator()
		 */
		public IInformationControlCreator getInformationControlCreator() {
			return null;
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.ICompletionProposalExtension3#getPrefixCompletionText(IDocument, int)
		 */
		public CharSequence getPrefixCompletionText(IDocument document, int completionOffset) {
			return fPrefix + fString;
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.ICompletionProposalExtension3#getPrefixCompletionStart(IDocument, int)
		 */
		public int getPrefixCompletionStart(IDocument document, int completionOffset) {
			return fOffset - fPrefix.length();
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.ICompletionProposalExtension4#isAutoInsertable()
		 */
		public boolean isAutoInsertable() {
			return false;
		}
		
		/**
		 * Return cursor position of proposal replacement string.
		 */
		public int getCursorPosition() {
			return fNewPosition;
		}

		/**
		 * Returns the relevance of the proposal
		 */
		public int getRelevance() {
			return TextProposal.R_JSP_JSF_EL_VARIABLE_ATTRIBUTE_VALUE+10;
		}

	}

	/**
	 * Creates a new Seam EL completion proposal computer.
	 */
	public ELProposalProcessor() {
	}

	/*
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#computeCompletionProposals(org.eclipse.jface.text.ITextViewer, int)
	 */
	public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer, int offset) {
		if(viewer == null) {
			return NO_PROPOSALS;
		}
		IDocument document = viewer.getDocument();
		int start = 0;
		int end = document.getLength();

		int[] region = getRegion(document, offset);
		if(region != null) {
			start = region[0];
			end = region[1];
		}
		
		return computeCompletionProposals(viewer, offset, start, end);
	}

	private int[] getRegion(IDocument document, int offset) {
		IStructuredModel model = StructuredModelManager.getModelManager().getExistingModelForRead(document);

		try {
		Document xmlDocument = (model instanceof IDOMModel) ? ((IDOMModel) model)
					.getDocument()
					: null;
			if (xmlDocument == null)
				return null;

			Node n = Utils.findNodeForOffset(xmlDocument, offset);

			if (n == null || !(n instanceof Attr || n instanceof Text))
				return null;

			int start = 0;
			int end = document.getLength();
			if (n instanceof IDOMNode) {
				start = ((IDOMNode) n).getStartOffset();
				end = ((IDOMNode) n).getEndOffset();
			}
			return new int[]{start, end};
		} finally {
			if(model != null) {
				model.releaseFromRead();
				model = null;
			}
		}
	}

	abstract protected boolean isEnabled(IFile file);

	abstract protected Image getImage();

	/**
	 * 
	 * @param start  start of relevant region in document
	 * @param end    end of relevant region in document
	 * 
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#computeCompletionProposals(org.eclipse.jface.text.ITextViewer, int)
	 */
	public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer, int offset, int start, int end) {
		try {
			ITextEditor part = getActiveEditor();
			if (part == null) {
				return NO_PROPOSALS;
			}

			IEditorInput editorInput = part.getEditorInput();
			if (!(editorInput instanceof IFileEditorInput)) {
				return NO_PROPOSALS;
			}

			IFile file = ((IFileEditorInput)editorInput).getFile();
			if (!isEnabled(file)) {
				return NO_PROPOSALS;
			}

			//TODO Now this will work only for EL. 
			//     If we need CA for expressions/variables without #{}, it should be handled separately.

			boolean isInEl = checkStartPositionInEL(viewer, offset);
			String prefix= getPrefix(viewer, offset, start, end);

			// JBIDE-3290 Quick-fix - this supresses the EL-proposals suggestions
			// in case of no "#{"-prefix is typed

			if (!isInEl) {
				return NO_PROPOSALS;
			}

			prefix = (prefix == null ? "" : prefix); //$NON-NLS-1$

			String proposalPrefix = ""; //$NON-NLS-1$
			String proposalSufix = ""; //$NON-NLS-1$
			String elStartChar = "#"; //$NON-NLS-1$
			String documentContent = null;
			IDocument document = viewer.getDocument();
			if (!isInEl) {
				// Work only with attribute value for JSP/HTML
				if((part instanceof XMLTextEditor) || (!isAttributeValue(viewer, offset))) {
					return NO_PROPOSALS;
				}
				prefix = ""; // Clear prefix because it's not the part of EL  //$NON-NLS-1$
				if(isCharSharp(viewer, offset-1) || isCharDollar(viewer, offset-1)) {
					proposalPrefix = "{";  //$NON-NLS-1$
					if (isCharDollar(viewer, offset-1)) {
						elStartChar = "$"; //$NON-NLS-1$
					}
				} else {
					proposalPrefix = "#{";  //$NON-NLS-1$
				}

				if(document != null) {
					documentContent = document.get(0,offset) +  proposalPrefix + document.get(offset, document.getLength() - offset);
				}
			} else {
				if(viewer.getDocument() != null) {
					documentContent = document.get();
				}
			}

			// Is '}'-bracket exists? If not - add the 
    		if(getELEndPosition(offset + proposalPrefix.length(), documentContent) == -1) {
    			proposalSufix = "}"; //$NON-NLS-1$
    		}

			ELResolver[] resolvers = ELResolverFactoryManager.getInstance().getResolvers(file);

			List<ICompletionProposal> resultList = new ArrayList<ICompletionProposal>();
			for (int i = 0; i < resolvers.length; i++) {

				if(isInEl && (prefix == null || prefix.length() == 0)) {
					String el = viewer.getDocument().get().substring(start, offset) + "a"; //$NON-NLS-1$
					ELModel model = resolvers[i].getParserFactory().createParser().parse(el);
					ELInvocationExpression expr = ELUtil.findExpression(model, el.length());
					if(expr == null) {
						break;
					}
				}

				List<Var> vars = ElVarSearcher.findAllVars(viewer, offset, resolvers[i].getParserFactory());

				SimpleELContext elContext = new SimpleELContext();
				elContext.setResource(file);
				elContext.setVars(vars);
				List<TextProposal> suggestions = resolvers[i].getProposals(elContext, prefix, offset);
				List<TextProposal> uniqueSuggestions = AbstractELCompletionEngine.makeProposalsUnique(suggestions);

				for (TextProposal kbProposal : uniqueSuggestions) {
					String string = kbProposal.getReplacementString();
					Image image = kbProposal.hasImage() ?
							kbProposal.getImage() : 
							getImage();

					if (string.length() >= 0) {
						string = proposalPrefix + string + proposalSufix;
						if (string.length() > 0 && ('#' == string.charAt(0) || '$' == string.charAt(0)))
	                		string = elStartChar + string.substring(1);

						if (string.startsWith("['") && string.endsWith("']") && prefix != null && prefix.endsWith(".")) {  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
							String newPrefix = prefix.substring(0, prefix.length() - 1);
							resultList.add(new Proposal(string, prefix, newPrefix, offset, offset - 1 + string.length() - proposalSufix.length(), image));
						} else {
							resultList.add(new Proposal(string, prefix, offset, offset + string.length() - proposalSufix.length(), image));
						}
					}
				}
			}
			if (resultList.isEmpty()) {
				return NO_PROPOSALS;
			}

			ICompletionProposal[] resultArray = new ICompletionProposal[resultList.size()];
			resultArray = resultList.toArray(resultArray);
			Arrays.sort(resultArray, new Comparator<ICompletionProposal>() {
				public int compare(ICompletionProposal arg0,
						ICompletionProposal arg1) {
					String str0 = (arg0 == null ? "" : arg0.getDisplayString()); //$NON-NLS-1$
					String str1 = (arg1 == null ? "" : arg1.getDisplayString()); //$NON-NLS-1$
					return str0.compareTo(str1);
				}});
			return resultArray;
		} catch (BadLocationException x) {
			ElUiPlugin.getDefault().logError(x);
			return NO_PROPOSALS;
		} catch (StringIndexOutOfBoundsException e) {
			ElUiPlugin.getDefault().logError(e);
			return NO_PROPOSALS;
		}
	}

	/**
	 * Calculates the EX expression operand string
	 * 
	 * @param viewer
	 * @param offset
	 * @param start  start of relevant region in document
	 * @param end    end of relevant region in document
	 * @return
	 * @throws BadLocationException
	 */
	public String getPrefix(ITextViewer viewer, int offset, int start, int end) throws StringIndexOutOfBoundsException {
		IDocument doc= viewer.getDocument();
		if (doc == null || offset > doc.getLength())
			return null;
		return getPrefix(doc, offset, start, end);
	}

	/**
	 * Calculates the EX expression operand string
	 * 
	 * @param viewer
	 * @param offset
	 * @param start  start of relevant region in document
	 * @param end    end of relevant region in document
	 * @return
	 * @throws StringIndexOutOfBoundsException
	 */
	public String getPrefix(IDocument document, int offset, int start, int end) throws StringIndexOutOfBoundsException {
		if (document == null || document.get() == null || offset > document.get().length())
			return null;
		ELInvocationExpression expr = AbstractELCompletionEngine.findExpressionAtOffset(document, offset, start, end);
		if (expr == null)
			return null;
		return document.get().substring(expr.getStartPosition(), offset);
	}

	private boolean isAttributeValue(ITextViewer viewer, int offset) {
		IndexedRegion treeNode = ContentAssistUtils.getNodeAt(viewer, offset);

		if(treeNode instanceof Node) {
			Node node = (Node) treeNode;
			while ((node != null) && (node.getNodeType() == Node.TEXT_NODE) && (node.getParentNode() != null)) {
				node = node.getParentNode();
			}
			if(node instanceof IDOMNode) {
				IDOMNode xmlnode = (IDOMNode) node;
				ITextRegion completionRegion = getCompletionRegion(offset, node);
				if (DOMRegionContext.XML_TAG_ATTRIBUTE_VALUE != completionRegion.getType())
					return false;
				ITextRegion nextRegion = getCompletionRegion(offset + 1, node);
				return DOMRegionContext.XML_TAG_ATTRIBUTE_VALUE == nextRegion.getType();
			}
		}
		return false;
	}

	/*
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#computeContextInformation(org.eclipse.jface.text.ITextViewer, int)
	 */
	public IContextInformation[] computeContextInformation(ITextViewer viewer, int offset) {
		// no context informations for Seam EL completions
		return NO_CONTEXTS;
	}
	
	private char[] autoActivChars;

	/*
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#getCompletionProposalAutoActivationCharacters()
	 */
	public char[] getCompletionProposalAutoActivationCharacters() {
		if(autoActivChars==null) {
			IPreferenceStore store= EditorsUI.getPreferenceStore();
			String superDefaultChars = store.getDefaultString(PreferenceConstants.CODEASSIST_AUTOACTIVATION_TRIGGERS_JAVA);
			StringBuffer redhatDefaultChars = new StringBuffer(superDefaultChars);
			if(superDefaultChars.indexOf("{")<0) { //$NON-NLS-1$
				redhatDefaultChars.append('{');
			}
			if(superDefaultChars.indexOf(".")<0) { //$NON-NLS-1$
				redhatDefaultChars.append('.');
			}
			if(superDefaultChars.indexOf("[")<0) { //$NON-NLS-1$
				redhatDefaultChars.append('[');
			}

			autoActivChars = new char[redhatDefaultChars.length()];
			redhatDefaultChars.getChars(0, redhatDefaultChars.length(), autoActivChars, 0);
			store.setDefault(PreferenceConstants.CODEASSIST_AUTOACTIVATION_TRIGGERS_JAVA, redhatDefaultChars.toString());
			store.setValue(PreferenceConstants.CODEASSIST_AUTOACTIVATION_TRIGGERS_JAVA, redhatDefaultChars.toString());
		}
		return autoActivChars;
	}

	/*
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#getContextInformationAutoActivationCharacters()
	 */
	public char[] getContextInformationAutoActivationCharacters() {
		return null;
	}
	
	/*
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#getContextInformationValidator()
	 */
	public IContextInformationValidator getContextInformationValidator() {
		return null;
	}

	/*
	 * @see org.eclipse.jface.text.contentassist.ICompletionProposalComputer#getErrorMessage()
	 */
	public String getErrorMessage() {
		return null; // no custom error message
	}

	/*
	 * Returns active text editor
	 * @return
	 */
	private ITextEditor getActiveEditor() {
		IWorkbenchWindow window= PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		if (window != null) {
			IWorkbenchPage page= window.getActivePage();
			if (page != null) {
				IEditorPart editor= page.getActiveEditor();
				if (editor instanceof IEditorWrapper)
					editor = ((IEditorWrapper) editor).getEditor();
				
				if (editor instanceof ITextEditor)
					return (ITextEditor) editor;
				else 
					return (ITextEditor)editor.getAdapter(ITextEditor.class);
			}
		}
		return null;
	}

	private boolean isCharSharp(ITextViewer viewer, int offset) throws BadLocationException {
		IDocument doc= viewer.getDocument();
		if (doc == null || offset > doc.getLength() || offset < 0) {
			return false;
		}

		return '#' == doc.getChar(offset);
	}

	private boolean isCharDollar(ITextViewer viewer, int offset) throws BadLocationException {
		IDocument doc= viewer.getDocument();
		if (doc == null || offset > doc.getLength() || offset < 0) {
			return false;
		}

		return '$' == doc.getChar(offset);
	}

	/*
	 * Checks if the EL start starting characters are present
	 * @param viewer
	 * @param offset
	 * @return
	 * @throws BadLocationException
	 */
	private boolean checkStartPositionInEL(ITextViewer viewer, int offset) throws BadLocationException {
		
		// JBIDE-1676: Do not even try to get IStructuredDocument in case of not-a-structured document
		// This will prevent class cast exceptions while using the editors other than StructuredEditor 
		// (CompilationUnitEditor, for example). 
		IStructuredDocumentRegion sdRegion = 
			(viewer.getDocument() instanceof IStructuredDocument ? 
					ContentAssistUtils.getStructuredDocumentRegion(viewer, offset) : 
						null);
		ITextRegion region = (sdRegion == null ? null : sdRegion.getRegionAtCharacterOffset(offset));
		
		int startIndex = (region == null ? 0 : sdRegion.getStartOffset() + region.getStart());
		
		IDocument doc= viewer.getDocument();
		if (doc == null || offset > doc.getLength())
			return false;
		int quotaCount = 0;
		while (--offset >= startIndex) {
			if ('}' == doc.getChar(offset))
				return false;

			if ('"' == doc.getChar(offset) || '\'' == doc.getChar(offset)) {
				int backslashCount = 0;
				while (doc.getChar(offset - 1 - backslashCount) == '\\') {
					backslashCount++;
				}
				if (backslashCount%2 == 0)
					quotaCount++;
			}

			if ('{' == doc.getChar(offset) &&
					(offset - 1) >= 0 && 
					('#' == doc.getChar(offset - 1) || 
							'$' == doc.getChar(offset - 1))) {
				if(quotaCount % 2 == 0) {
					return true;
				}
			}
		}
		return false;
	}
	
	/*
	 * Checks if the EL operand ending character is present
	 * @return
	 */
	private int getELEndPosition(int initialOffset, String restOfCurrentValue) {
		int offset = -1;

		char inQuotesChar = 0;
		while (++offset < restOfCurrentValue.length() - initialOffset) {
			if (inQuotesChar == 0) {
				if ('}' == restOfCurrentValue.charAt(initialOffset + offset))
					return offset;

				if ('#' == restOfCurrentValue.charAt(initialOffset + offset))
					return -1;

				if ('<' == restOfCurrentValue.charAt(initialOffset + offset))
					return -1;

				if ('>' == restOfCurrentValue.charAt(initialOffset + offset))
					return -1;

				if ('/' == restOfCurrentValue.charAt(initialOffset + offset) &&
					(initialOffset + offset + 1 < restOfCurrentValue.length() &&
					'>' == restOfCurrentValue.charAt(initialOffset + offset + 1)))
					return -1;

				if ('"' == restOfCurrentValue.charAt(initialOffset + offset) || 
                				'\'' == restOfCurrentValue.charAt(initialOffset + offset)) {
					inQuotesChar = restOfCurrentValue.charAt(initialOffset + offset);
				}

				if ('\\' == restOfCurrentValue.charAt(initialOffset + offset)) {
	                int backslashCount = 1;
	                
	                while ((initialOffset + offset + backslashCount) < restOfCurrentValue.length() && 
	                		restOfCurrentValue.charAt(initialOffset + offset + backslashCount) == '\\') {
	                    backslashCount++;
	                }

	                if (initialOffset + offset + backslashCount >= restOfCurrentValue.length())
	                	return -1;
	                
	                if (backslashCount % 2 == 1 && 
	                		('"' == restOfCurrentValue.charAt(initialOffset + offset + backslashCount) || 
	                				'\'' == restOfCurrentValue.charAt(initialOffset + offset + backslashCount))) {
	                    inQuotesChar = restOfCurrentValue.charAt(initialOffset + offset + backslashCount);
	                    offset += backslashCount;
	                }
				}
			} else {
				if ('"' == restOfCurrentValue.charAt(initialOffset + offset) || 
        				'\'' == restOfCurrentValue.charAt(initialOffset + offset)) {
					inQuotesChar = 0;
				}

				if ('\\' == restOfCurrentValue.charAt(initialOffset + offset)) {
	                int backslashCount = 1;
	                
	                while ((initialOffset + offset + backslashCount) < restOfCurrentValue.length() && 
	                		restOfCurrentValue.charAt(initialOffset + offset + backslashCount) == '\\') {
	                    backslashCount++;
	                }

	                if (initialOffset + offset + backslashCount >= restOfCurrentValue.length())
	                	return -1;
	                
	                if (backslashCount % 2 == 1 && 
	                		('"' == restOfCurrentValue.charAt(initialOffset + offset + backslashCount) || 
	                				'\'' == restOfCurrentValue.charAt(initialOffset + offset + backslashCount))) {
	                    inQuotesChar = 0;
	                    offset += backslashCount;
	                }
				}
			}
		}
		return -1;
	}
}