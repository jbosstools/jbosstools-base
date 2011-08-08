/*******************************************************************************
 * Copyright (c) 2007-2011 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.text.xml;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.hyperlink.IHyperlinkDetector;
import org.eclipse.jface.text.information.IInformationProvider;
import org.eclipse.jface.text.quickassist.IQuickAssistAssistant;
import org.eclipse.jface.text.quickassist.QuickAssistAssistant;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.wst.sse.ui.StructuredTextViewerConfiguration;
import org.eclipse.wst.sse.ui.contentassist.CompletionProposalInvocationContext;
import org.eclipse.wst.sse.ui.internal.ExtendedConfigurationBuilder;
import org.eclipse.wst.sse.ui.internal.SSEUIPlugin;
import org.eclipse.wst.sse.ui.internal.derived.HTMLTextPresenter;
import org.eclipse.wst.sse.ui.internal.preferences.EditorPreferenceNames;
import org.eclipse.wst.sse.ui.internal.taginfo.AnnotationHoverProcessor;
import org.eclipse.wst.sse.ui.internal.taginfo.TextHoverManager;
import org.eclipse.wst.sse.ui.internal.util.EditorUtility;
import org.eclipse.wst.xml.ui.StructuredTextViewerConfigurationXML;
import org.eclipse.wst.xml.ui.internal.contentassist.XMLStructuredContentAssistProcessor;
import org.jboss.tools.common.text.xml.contentassist.ProposalSorter;
import org.jboss.tools.common.text.xml.info.ChainTextHover;
import org.jboss.tools.common.text.xml.xpl.MarkerProblemAnnotationHoverProcessor;
import org.jboss.tools.common.text.xml.info.TextHoverInformationProvider;
/**
 * @author Igels
 */
@SuppressWarnings("restriction")
public class XMLTextViewerConfiguration extends StructuredTextViewerConfigurationXML {
	private static final char[] PROPOSAL_AUTO_ACTIVATION_CHARS = new char[] {
		'<', '=', '"', '\'', '.', '{', '['
	};

	SourceViewerConfiguration initial = null;

	public XMLTextViewerConfiguration() {
		super();
	}
	
	public void setInitialConfiguration(SourceViewerConfiguration initial) {
		this.initial = initial;
	}

	protected IContentAssistProcessor[] getContentAssistProcessors(ISourceViewer sourceViewer, String partitionType) {
		
//		IContentAssistProcessor[] superProcessors = super.getContentAssistProcessors(
//				sourceViewer, partitionType);
		
		IContentAssistProcessor superProcessor = new XMLStructuredContentAssistProcessor(
				this.getContentAssistant(), partitionType, sourceViewer) {

					@SuppressWarnings({ "rawtypes", "unchecked" })
					@Override
					protected List filterAndSortProposals(List proposals,
							IProgressMonitor monitor,
							CompletionProposalInvocationContext context) {
						return ProposalSorter.filterAndSortProposals(proposals, monitor, context);
					}

					@Override
					public char[] getCompletionProposalAutoActivationCharacters() {
						char[] superAutoActivationCharacters = super.getCompletionProposalAutoActivationCharacters();
						if (superAutoActivationCharacters == null)
							return PROPOSAL_AUTO_ACTIVATION_CHARS;
						
						String chars = new String(superAutoActivationCharacters);
						for (char ch : PROPOSAL_AUTO_ACTIVATION_CHARS) {
							if (chars.indexOf(ch) == -1) {
								chars += ch;
							}
						}
						return chars.toCharArray();
					}
		};
		
		List<IContentAssistProcessor> processors = new ArrayList<IContentAssistProcessor>();

//		SortingCompoundContentAssistProcessor sortingCompoundProcessor = new SortingCompoundContentAssistProcessor(sourceViewer, partitionType);
//		if (sortingCompoundProcessor.supportsPartitionType(partitionType)) {
//			processors.add(sortingCompoundProcessor);
//		}
//		processors.addAll(Arrays.asList(superProcessors));
		processors.add(superProcessor);
		return processors.toArray(new IContentAssistProcessor[0]);
	}

	/*
	 * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getHyperlinkDetectors(org.eclipse.jface.text.source.ISourceViewer)
	 * @since 3.1
	 */
	public IHyperlinkDetector[] getHyperlinkDetectors(ISourceViewer sourceViewer) {
		List<IHyperlinkDetector> allDetectors = new ArrayList<IHyperlinkDetector>(0);

		IHyperlinkDetector extHyperlinkDetector = getTextEditorsExtensionsHyperlinkDetector(); 

		if (extHyperlinkDetector != null) allDetectors.add(extHyperlinkDetector);
		
		IHyperlinkDetector[] superDetectors = super.getHyperlinkDetectors(sourceViewer);
		for (IHyperlinkDetector detector : superDetectors) {
			if (!allDetectors.contains(detector)) {
				allDetectors.add(detector);
			}
		}
		
		return allDetectors.toArray(new IHyperlinkDetector[0]);
	}

	@SuppressWarnings("deprecation")
	private IHyperlinkDetector getTextEditorsExtensionsHyperlinkDetector() {
		Plugin plugin = Platform.getPlugin("org.jboss.tools.common.text.ext"); //$NON-NLS-1$
		return (plugin != null && plugin instanceof IAdaptable ? (IHyperlinkDetector)((IAdaptable)plugin).getAdapter(IHyperlinkDetector.class):null);
	}
	
	/**
	 * @deprecated
	 */
	IContentAssistProcessor[] getInitialProcessors(ISourceViewer sourceViewer, String partitionType) {
		if(initial == null) return null;
		//method getContentAssistProcessors() is declared in StructuredTextViewerConfiguration
		//and its subclasses
		if(!(initial instanceof StructuredTextViewerConfiguration)) return null;
		try {
			Method m = findDeclaredMethod(initial.getClass(), "getContentAssistProcessors", new Class[]{ISourceViewer.class, String.class}); //$NON-NLS-1$
			if(m == null) return null;
			m.setAccessible(true);
			return (IContentAssistProcessor[])m.invoke(initial, new Object[]{sourceViewer, partitionType});
		} catch (IllegalArgumentException e) {
			XmlEditorPlugin.getPluginLog().logError(e);
		} catch (IllegalAccessException e) {
			XmlEditorPlugin.getPluginLog().logError(e);
		} catch (InvocationTargetException e) {
			XmlEditorPlugin.getPluginLog().logError(e);
		}
		
		return null;
	}

	/**
	 * @deprecated
	 */
	@SuppressWarnings("rawtypes")
	private Method findDeclaredMethod(Class cls, String name, Class[] paramTypes) {
		Method[] ms = cls.getDeclaredMethods();
		if (ms != null) for (int i = 0; i < ms.length; i++) {
			if(!ms[i].getName().equals(name)) continue;
			Class<?>[] ps = ms[i].getParameterTypes();
			if(ps == null || ps.length != paramTypes.length) continue;
			boolean equal = true;
			for (int j = 0; j < ps.length && equal; j++) {
				if(!ps[j].getName().equals(paramTypes[j].getName())) equal = false;
			}
			if(!equal) continue;
			return ms[i];
		}
		Class<?> sc = cls.getSuperclass();
		if(sc == null || sc == Object.class) 
			return null;
		return findDeclaredMethod(sc, name, paramTypes);
	}

	/**
	 * Create documentation hovers based on hovers contributed via
	 * <code>org.eclipse.wst.sse.ui.editorConfiguration</code> extension point
	 * 
	 * Copied from
	 * {@link org.eclipse.wst.sse.ui.StructuredTextViewerConfiguration} because
	 * of private modifier
	 * 
	 * @param partitionType
	 * @return
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	private ITextHover[] createDocumentationHovers(String partitionType) {
		List extendedTextHover = ExtendedConfigurationBuilder.getInstance()
				.getConfigurations(
						ExtendedConfigurationBuilder.DOCUMENTATIONTEXTHOVER,
						partitionType);
		return (ITextHover[]) extendedTextHover
				.toArray(new ITextHover[extendedTextHover.size()]);
	}
	
	@Override
	protected IInformationProvider getInformationProvider(
			ISourceViewer sourceViewer, String partitionType) {
		ITextHover[] hovers = createDocumentationHovers(partitionType);
		if (hovers == null) {
			hovers = new ITextHover[] {new ChainTextHover(
					new ITextHover[0])};
		}
		
		return new TextHoverInformationProvider(new ChainTextHover(
				hovers));
	}
	
	@Override
	public ITextHover getTextHover(ISourceViewer sourceViewer,
			String contentType, int stateMask) {
		ITextHover textHover = null;

		/*
		 * Returns a default problem, annotation, and best match hover depending
		 * on stateMask
		 */
		TextHoverManager.TextHoverDescriptor[] hoverDescs = SSEUIPlugin
				.getDefault().getTextHoverManager().getTextHovers();
		int i = 0;
		while (i < hoverDescs.length && textHover == null) {
			if (hoverDescs[i].isEnabled()
					&& computeStateMask(hoverDescs[i].getModifierString()) == stateMask) {
				String hoverType = hoverDescs[i].getId();
				if (TextHoverManager.PROBLEM_HOVER.equalsIgnoreCase(hoverType))
//					textHover = new ProblemAnnotationHoverProcessor();
					textHover = new MarkerProblemAnnotationHoverProcessor();
				else if (TextHoverManager.ANNOTATION_HOVER
						.equalsIgnoreCase(hoverType))
					textHover = new AnnotationHoverProcessor();
				else if (TextHoverManager.COMBINATION_HOVER
						.equalsIgnoreCase(hoverType)) {
					textHover = new ChainTextHover(createDocumentationHovers(contentType));
				} else if (TextHoverManager.DOCUMENTATION_HOVER
						.equalsIgnoreCase(hoverType)) {
					ITextHover[] hovers = createDocumentationHovers(contentType);
					if (hovers.length > 0) {
						textHover = hovers[0];
					}
				}
			}
			i++;
		}
		return textHover;
	}
	
	private IQuickAssistAssistant fQuickAssistant = null;
	
	private Color getColor(String key) {
		RGB rgb = PreferenceConverter.getColor(fPreferenceStore, key);
		return EditorUtility.getColor(rgb);
	}
	
	@Override
	public IQuickAssistAssistant getQuickAssistAssistant(ISourceViewer sourceViewer) {
		if (fQuickAssistant == null) {
			IQuickAssistAssistant assistant = new QuickAssistAssistant();
			assistant.setQuickAssistProcessor(new JBDSQuickAssistProcessor());
			assistant.setInformationControlCreator(getQuickAssistAssistantInformationControlCreator());

			if (fPreferenceStore != null) {
				Color color = getColor(EditorPreferenceNames.CODEASSIST_PROPOSALS_BACKGROUND);
				assistant.setProposalSelectorBackground(color);

				color = getColor(EditorPreferenceNames.CODEASSIST_PROPOSALS_FOREGROUND);
				assistant.setProposalSelectorForeground(color);
			}
			fQuickAssistant = assistant;
		}
		return fQuickAssistant;
	}
	
	/**
	 * Returns the information control creator for the quick assist assistant.
	 * 
	 * @return the information control creator
	 */
	private IInformationControlCreator getQuickAssistAssistantInformationControlCreator() {
		return new IInformationControlCreator() {
			public IInformationControl createInformationControl(Shell parent) {
				return new DefaultInformationControl(parent, new HTMLTextPresenter(true));
			}
		};
	}

}
