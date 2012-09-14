/*******************************************************************************
 * Copyright (c) 2007-2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.common.el.ui.ca;

import java.io.Reader;
import java.io.StringReader;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.text.javadoc.JavadocContentAccess2;
import org.eclipse.jdt.internal.ui.viewsupport.JavaElementLinks;
import org.eclipse.jdt.ui.JavaElementLabels;
import org.eclipse.jdt.ui.PreferenceConstants;
import org.eclipse.jface.internal.text.html.HTMLPrinter;
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
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.wst.sse.core.internal.provisional.IStructuredModel;
import org.eclipse.wst.sse.core.internal.provisional.StructuredModelManager;
import org.eclipse.wst.sse.ui.internal.contentassist.IRelevanceCompletionProposal;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMModel;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMNode;
import org.eclipse.wst.xml.ui.internal.contentassist.AbstractContentAssistProcessor;
import org.eclipse.wst.xml.ui.internal.util.SharedXMLEditorPluginImageHelper;
import org.jboss.tools.common.el.core.ELReference;
import org.jboss.tools.common.el.core.ca.AbstractELCompletionEngine;
import org.jboss.tools.common.el.core.ca.ELTextProposal;
import org.jboss.tools.common.el.core.ca.MessagesELTextProposal;
import org.jboss.tools.common.el.core.model.ELInvocationExpression;
import org.jboss.tools.common.el.core.resolver.ELContext;
import org.jboss.tools.common.el.core.resolver.ELResolver;
import org.jboss.tools.common.el.core.resolver.ELResolverFactoryManager;
import org.jboss.tools.common.el.ui.ElUiPlugin;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.impl.JarSystemImpl;
import org.jboss.tools.common.text.TextProposal;
import org.jboss.tools.common.text.ext.util.Utils;
import org.jboss.tools.common.ui.CommonUIPlugin;
import org.jboss.tools.common.util.EclipseUIUtil;
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
		private String fDisplayString;
		private String fAlternateMatch;
		private String fAdditionalProposalInfo;
		private IJavaElement[] fJavaElements;
		private MessagesELTextProposal fPropertySource;

		private Image fImage;

		public Proposal(String string, String prefix, int offset) {
			this(string, prefix, offset, offset + string.length());
		}

		public Proposal(String string, String prefix, int offset, int newPosition) {
			this(string, prefix, prefix, offset, offset + string.length(), null, null, null, null, null, null);
		}

/**/	public Proposal(String string, String prefix, int offset, int newPosition, Image image, String displayString, String alternateMatch, String additionalProposalInfo, IJavaElement[] javaElements, MessagesELTextProposal propertySource) {
			this(string, prefix, prefix, offset, offset + string.length(), image, displayString, alternateMatch, additionalProposalInfo, javaElements, propertySource);
		}

		public Proposal(String string, String prefix, String newPrefix, int offset, int newPosition) {
			this(string, prefix, newPrefix, offset, newPosition, null, null, null, null, null, null);
		}

/**/	public Proposal(String string, String prefix, String newPrefix, int offset, int newPosition, Image image, String displayString, String alternateMatch, String additionalProposalInfo, IJavaElement[] javaElements, MessagesELTextProposal propertySource) {
			fString = string;
			fPrefix = prefix;
			fNewPrefix = newPrefix;
			fOffset = offset;
			fNewPosition = newPosition;
			fImage = image;
			fDisplayString = displayString;
			fAlternateMatch = alternateMatch;
			fAdditionalProposalInfo = additionalProposalInfo;
			fJavaElements = javaElements;
			if (fJavaElements != null && fJavaElements.length > 0) 
				fAdditionalProposalInfo = null; // Drop it due to valculate it later based on java elements
			fPropertySource = propertySource;
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
			return new Point(getCursorPosition(), 0);
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.ICompletionProposal#getAdditionalProposalInfo()
		 */
		public String getAdditionalProposalInfo() {
			if (fAdditionalProposalInfo == null) {
				if (this.fJavaElements != null && this.fJavaElements.length > 0) {
					this.fAdditionalProposalInfo = extractProposalContextInfo(fJavaElements);
				} else if (fPropertySource != null) {
					this.fAdditionalProposalInfo = extractProposalContextInfo(fPropertySource);
				}
			}
			if (fAdditionalProposalInfo == null) 
				fAdditionalProposalInfo = ""; // This is to prevent repeat of extracting Java Info //$NON-NLS-1$
			return fAdditionalProposalInfo;
		}

		/*
		 * Extracts the additional proposal information based on Javadoc for the stored IJavaElement objects
		 */
		private String extractProposalContextInfo(IJavaElement[] elements) {
			int nResults= elements.length;
			StringBuffer buffer= new StringBuffer();
			boolean hasContents= false;
			IJavaElement element= null;

			if (nResults > 1) {
	/*			for (int i= 0; i < elements.length; i++) {
					if (elements[i] == null) continue;
					if (elements[i] instanceof IMember || 
							elements[i].getElementType() == IJavaElement.LOCAL_VARIABLE || 
							elements[i].getElementType() == IJavaElement.TYPE_PARAMETER) {
						buffer.append('\uE467').append(' ').append(getInfoText(elements[i]));
						hasContents= true;
					}
					buffer.append("<br/>"); //$NON-NLS-1$
				}
	*/
				for (int i=0; i < elements.length; i++) {
					if (elements[i] == null) continue;
					if (elements[i] instanceof IMember || 
							elements[i].getElementType() == IJavaElement.LOCAL_VARIABLE || 
							elements[i].getElementType() == IJavaElement.TYPE_PARAMETER) {
						buffer.append('\uE467').append(' ');
						addFullInfo(buffer, elements[i]);
						buffer.append("<br/>"); //$NON-NLS-1$
						hasContents = true;
					}
				}
			} else {
				element= elements[0];
				if (element instanceof IMember ||
						element.getElementType() == IJavaElement.LOCAL_VARIABLE || 
						element.getElementType() == IJavaElement.TYPE_PARAMETER) {
					addFullInfo(buffer, element);
					hasContents= true;
				}
			}

			if (!hasContents)
				return null;

			if (buffer.length() > 0) {
				HTMLPrinter.insertPageProlog(buffer, 0, (String)null);
				HTMLPrinter.addPageEpilog(buffer);
				return buffer.toString();
			}

			return null;
		}

		/*
		 * Extracts the additional proposal information based on Javadoc for the stored IJavaElement objects
		 */
		private String extractProposalContextInfo(MessagesELTextProposal propertySource) {
			String propertyName = propertySource.getPropertyName();
			String baseName = propertySource.getBaseName();
			List<XModelObject> objects = (List<XModelObject>)propertySource.getAllObjects();
			
			String info = getELMessagesHoverInternal(baseName, propertyName, objects);
			if (info == null) return null;

			StringBuffer buffer= new StringBuffer(info);
			HTMLPrinter.insertPageProlog(buffer, 0, (String)null);
			HTMLPrinter.addPageEpilog(buffer);
			return buffer.toString();
		}

		private static final long LABEL_FLAGS=  JavaElementLabels.ALL_FULLY_QUALIFIED
				| JavaElementLabels.M_PRE_RETURNTYPE | JavaElementLabels.M_PARAMETER_TYPES | JavaElementLabels.M_PARAMETER_NAMES | JavaElementLabels.M_EXCEPTIONS
				| JavaElementLabels.F_PRE_TYPE_SIGNATURE | JavaElementLabels.M_PRE_TYPE_PARAMETERS | JavaElementLabels.T_TYPE_PARAMETERS
				| JavaElementLabels.USE_RESOLVED;
		private static final long LOCAL_VARIABLE_FLAGS= LABEL_FLAGS & ~JavaElementLabels.F_FULLY_QUALIFIED | JavaElementLabels.F_POST_QUALIFIED;
		private static final long TYPE_PARAMETER_FLAGS= LABEL_FLAGS | JavaElementLabels.TP_POST_QUALIFIED;

		/* 
		 * Returns the label for the IJavaElement objects
		 */
		private String getInfoText(IJavaElement element) {
			long flags;
			switch (element.getElementType()) {
				case IJavaElement.LOCAL_VARIABLE:
					flags= LOCAL_VARIABLE_FLAGS;
					break;
				case IJavaElement.TYPE_PARAMETER:
					flags= TYPE_PARAMETER_FLAGS;
					break;
				default:
					flags= LABEL_FLAGS;
					break;
			}
			StringBuffer label= new StringBuffer(JavaElementLinks.getElementLabel(element, flags));
		
			// The following lines were commented out because of JBIDE-8923 faced in Eclipse 3.7
			//
//				StringBuffer buf= new StringBuffer();
//				buf.append("<span style='word-wrap:break-word;'>"); //$NON-NLS-1$
//				buf.append(label);
//				buf.append("</span>"); //$NON-NLS-1$

//				return buf.toString();
			return label.toString();
		}

		/*
		 * Adds full information to the additional proposal information
		 * 
		 * @param buffer
		 * @param element
		 * @return
		 */
		private void addFullInfo(StringBuffer buffer, IJavaElement element) {
			if (element instanceof IMember) {
				IMember member= (IMember) element;
				HTMLPrinter.addSmallHeader(buffer, getInfoText(member));
				Reader reader = null;
				try {
					String content= JavadocContentAccess2.getHTMLContent(member, true);
					reader= content == null ? null : new StringReader(content);
				} catch (JavaModelException ex) {
					JavaPlugin.log(ex);
				}
				
				if (reader == null) {
					reader = new StringReader(Messages.NO_JAVADOC);
				}

				if (reader != null) {
					buffer.append("<br/>"); //$NON-NLS-1$
					buffer.append(HTMLPrinter.read(reader));
//						HTMLPrinter.addParagraph(buffer, reader);
				}

			} else if (element.getElementType() == IJavaElement.LOCAL_VARIABLE || element.getElementType() == IJavaElement.TYPE_PARAMETER) {
				HTMLPrinter.addSmallHeader(buffer, getInfoText(element));
			}
		}

		/*
		 * @see org.eclipse.jface.text.contentassist.ICompletionProposal#getDisplayString()
		 */
		public String getDisplayString() {
			if (fDisplayString != null)
				return fDisplayString;
			
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
				return offset >= fOffset && 
						((offset < fOffset + fString.length() && document.get(prefixStart, offset - (prefixStart)).equals((fPrefix + fString).substring(0, offset - prefixStart))) || 
								(fAlternateMatch != null && offset < fOffset + fAlternateMatch.length() && document.get(prefixStart, offset - (prefixStart)).equals((fPrefix + fAlternateMatch).substring(0, offset - prefixStart))));
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
			int cursorPosition = -1;

			int openingQuoteInReplacement = fString.lastIndexOf('(');
			int closingQuoteInReplacement = fString.lastIndexOf(')');
			int openingQuoteInDisplay = fDisplayString.lastIndexOf('(');
			int closingQuoteInDisplay = fDisplayString.lastIndexOf(')');

			if (openingQuoteInReplacement != -1
					&& closingQuoteInReplacement != -1
					&& openingQuoteInDisplay != -1
					&& closingQuoteInDisplay != -1
					&& (closingQuoteInReplacement - openingQuoteInReplacement) != (closingQuoteInDisplay - openingQuoteInDisplay)) {
				cursorPosition = openingQuoteInReplacement + 1;
			}

			return cursorPosition > -1 ? fOffset + cursorPosition : fNewPosition;
		}

		/**
		 * Returns the relevance of the proposal
		 */
		public int getRelevance() {
			return TextProposal.R_JSP_JSF_EL_VARIABLE_ATTRIBUTE_VALUE+10;
		}

		public String getfAlternateMatch() {
			return fAlternateMatch;
		}

		public void setfAlternateMatch(String fAlternateMatch) {
			this.fAlternateMatch = fAlternateMatch;
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

	abstract protected ELContext getELContext(IFile file);

	/**
	 * 
	 * @param start  start of relevant region in document
	 * @param end    end of relevant region in document
	 * 
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#computeCompletionProposals(org.eclipse.jface.text.ITextViewer, int)
	 */
	public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer, int offset, int start, int end) {
		ITextEditor part = EclipseUIUtil.getActiveEditor();
		if (part == null) {
			return NO_PROPOSALS;
		}

		IEditorInput editorInput = part.getEditorInput();
		if (!(editorInput instanceof IFileEditorInput)) {
			return NO_PROPOSALS;
		}

		IFile file = ((IFileEditorInput) editorInput).getFile();
		if (!isEnabled(file)) {
			return NO_PROPOSALS;
		}

		ELContext context = getELContext(file);
		if (context == null) {
			return NO_PROPOSALS;
		}

		ELReference ref = context.getELReference(offset);
		if(ref==null) {
			return NO_PROPOSALS;
		}

		ELResolver[] resolvers = ELResolverFactoryManager.getInstance().getResolvers(file);

		List<ICompletionProposal> resultList = new ArrayList<ICompletionProposal>();

		String prefix= getPrefix(viewer, offset, start, end);

		prefix = (prefix == null ? "" : prefix); //$NON-NLS-1$

		String proposalPrefix = ""; //$NON-NLS-1$
		String proposalSufix = ""; //$NON-NLS-1$
		String elStartChar = "#"; //$NON-NLS-1$
		String documentContent = ref.getELModel().getSource();
		// Is '#{', or '${', or '}'-bracket exists? If not - add the 
		int elEndPosition = documentContent.indexOf("#{", offset - ref.getStartPosition()); //$NON-NLS-1$
		int limit = documentContent.indexOf("${", offset - ref.getStartPosition()); //$NON-NLS-1$
		if (limit != -1 && elEndPosition != -1 && limit < elEndPosition) {
			elEndPosition = limit;
		}
		limit = documentContent.indexOf('}', offset - ref.getStartPosition());
		if (limit != -1 && (elEndPosition == -1 || (elEndPosition != -1 && limit < elEndPosition))) {
			elEndPosition = limit+1;
		}
		String restOfEL = elEndPosition == -1 ? "" : documentContent.substring(offset - ref.getStartPosition(), elEndPosition); //$NON-NLS-1$
		if(restOfEL.indexOf('}') == -1) {
			proposalSufix = "}"; //$NON-NLS-1$
		}

		for (int i = 0; i < resolvers.length; i++) {
			List<TextProposal> suggestions = resolvers[i].getProposals(context,
					offset);
			List<TextProposal> uniqueSuggestions = AbstractELCompletionEngine
					.makeProposalsUnique(suggestions);

			for (TextProposal kbProposal : uniqueSuggestions) {
				String string = kbProposal.getReplacementString();
				Image image = kbProposal.hasImage() ? CommonUIPlugin.getImageDescriptorRegistry().get(kbProposal.getImageDescriptor()):getImage();
				if (string.length() >= 0) {
					string = proposalPrefix + string;
					if (string.length() > 0 && ('#' == string.charAt(0) || '$' == string.charAt(0)))
                		string = elStartChar + string.substring(1);

					String additionalProposalInfo = kbProposal.getContextInfo();
					IJavaElement[] javaElements = null;
					MessagesELTextProposal source = null;
					
					if (kbProposal instanceof ELTextProposal) {
						javaElements = ((ELTextProposal)kbProposal).getAllJavaElements();
					} else if (kbProposal instanceof MessagesELTextProposal) {
						source = (MessagesELTextProposal)kbProposal;
					}

					if (string.startsWith("[") && prefix != null && prefix.indexOf('.') != -1) {  //$NON-NLS-1$
						String newPrefix = prefix.substring(0, prefix.lastIndexOf('.'));
						if (string.indexOf('\'') != -1 && restOfEL.indexOf('\'') != -1) // Exclude last quote if this char already exists
							string = string.substring(0, string.lastIndexOf('\''));
						
						if (string.indexOf(']') == -1 && restOfEL.indexOf(']') == -1) // Add closing square bracket if needed
							string += ']';
					
						string +=  proposalSufix;
						resultList.add(new Proposal(string, prefix, newPrefix, offset, offset - (prefix.length() - newPrefix.length()) + string.length() - proposalSufix.length(), image,
								kbProposal.getLabel(), kbProposal.getAlternateMatch(), additionalProposalInfo, javaElements, source));
					} else {
						if (string.indexOf('\'') != -1 && restOfEL.indexOf('\'') != -1) // Exclude last quote if this char already exists
							string = string.substring(0, string.lastIndexOf('\''));
						
						if ((string.indexOf('[') != -1 || (prefix.indexOf('[') != -1 && prefix.indexOf(']', prefix.lastIndexOf('[')) == -1)) 
								&& string.indexOf(']') == -1 && restOfEL.indexOf(']') == -1) // Add closing square bracket if needed
							string += ']';
							
						string +=  proposalSufix;
						resultList.add(new Proposal(string, prefix, offset, offset + string.length() - proposalSufix.length(), image, 
								kbProposal.getLabel(), kbProposal.getAlternateMatch(), additionalProposalInfo, javaElements, source));
					}
				}
			}
		}
		if (resultList.isEmpty()) {
			return NO_PROPOSALS;
		}

		ICompletionProposal[] resultArray = new ICompletionProposal[resultList
				.size()];
		resultArray = resultList.toArray(resultArray);
		Arrays.sort(resultArray, new Comparator<ICompletionProposal>() {
			public int compare(ICompletionProposal arg0,
					ICompletionProposal arg1) {
				return arg0.getDisplayString().compareTo(arg1.getDisplayString());
			}
		});
		return resultArray;
	}

	/*
	 * @return non-paired quote char if exists, otherwise 0
	 */
	private char getPreceedingQuoteChar(int initialOffset, String restOfCurrentValue) {
		int offset = initialOffset;

		char inQuotesChar = 0;
		while (--offset >= 0) {
			if ('"' == restOfCurrentValue.charAt(offset) || 
    				'\'' == restOfCurrentValue.charAt(offset) && 
    				(inQuotesChar == 0 || inQuotesChar == restOfCurrentValue.charAt(offset))) {
				if (initialOffset + offset > 0 && restOfCurrentValue.charAt(offset - 1) == '\\') {
	                int backslashCount = 1;
	                while ((offset - backslashCount) >= 0 && 
	                		restOfCurrentValue.charAt(offset - backslashCount) == '\\') {
	                    backslashCount++;
	                }
            
	                if (backslashCount % 2 == 1) {
	                    inQuotesChar = inQuotesChar == 0 ? restOfCurrentValue.charAt(offset) : 0;
	                    offset -= backslashCount;
	                }
				} else {
					inQuotesChar = inQuotesChar == 0 ? restOfCurrentValue.charAt(offset) : 0;
				}
			} else if ('{' == restOfCurrentValue.charAt(offset)) {
				if (offset > 0 && 
						('#' == restOfCurrentValue.charAt(offset -1) || 
						'$' == restOfCurrentValue.charAt(offset -1))) {
					return inQuotesChar;
				}
			}
		}
		return inQuotesChar;
	}
	
	/*
	 * Checks if the EL operand ending character is present
	 * @return
	 */
	private int getELEndPosition(int initialOffset, String restOfCurrentValue) {
		int offset = -1;

		char inQuotesChar = getPreceedingQuoteChar(initialOffset, restOfCurrentValue);
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
	
	public static String getELMessagesHoverInternal(String baseName, String propertyName, List<XModelObject> objects) {
		StringBuffer buffer= new StringBuffer();

		if (propertyName != null && propertyName.length() > 0) 
			buffer.append(MessageFormat.format(Messages.ELInfoHover_propertyName, 
				propertyName));
			
		if (baseName != null && baseName.length() > 0)
			buffer.append(MessageFormat.format(Messages.ELInfoHover_baseName, 
					baseName));
		
		if (objects != null) {
			boolean firstValue = true;
			for (XModelObject o : objects) {
				IFile propFile = (IFile)o.getAdapter(IFile.class);
				String propFilePath = null;
				if (propFile != null) {
					propFilePath = propFile.getFullPath().toString();
				} else {
					XModelObject parent = o.getFileType() == XModelObject.FILE ? o : o.getParent();
					String path = parent.getPath();
					while (parent != null && parent.getFileType() != XModelObject.SYSTEM) {
						parent = parent.getParent();
					}
					if (parent instanceof JarSystemImpl) {
						String sysPath = parent.getPath();
						path = path.substring(sysPath.length());
	
						String jarPath = ((JarSystemImpl) parent).getLocation();
						
						IResource jar = ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(new Path(jarPath));
						
						if (jar != null) {
							jarPath = jar.getFullPath().toString();
						}
						
						propFilePath = jarPath + "!" + path; //$NON-NLS-1$
					}
				}
				buffer.append(Messages.ELInfoHover_newLine);
				if (!firstValue) buffer.append(Messages.ELInfoHover_newLine);
				else firstValue = false;
				
				buffer.append(MessageFormat.format(Messages.ELInfoHover_resourceBundle, 
						propFilePath != null ? propFilePath : Messages.ELInfoHover_resourceBundleNotDefined));
				
				if (propertyName != null) {
					String value = o.get("VALUE");  //$NON-NLS-1$
					boolean addCut = false;
					if (value != null) {
						if (value.length() > 100) {
							// Get first words of value
							int lastSpace = value.lastIndexOf(' ', 99);
							if (lastSpace != -1) {
								value = value.substring(0, lastSpace);
							} else { // cut as is
								value = value.substring(0, 100);
							}
							addCut = true;
						}
					}
					buffer.append(Messages.ELInfoHover_newLine);
					buffer.append(MessageFormat.format(Messages.ELInfoHover_resourceBundlePropertyValue, 
							value != null ? value : Messages.ELInfoHover_resourceBundlePropertyValueNotDefined,
									addCut ? Messages.ELInfoHover_treeDots : "")); //$NON-NLS-1$
//					sb.append(Messages.ELInfoHover_newLine);
				}
			}
		}

		return buffer.length() == 0 ? null : buffer.toString();
	}

}