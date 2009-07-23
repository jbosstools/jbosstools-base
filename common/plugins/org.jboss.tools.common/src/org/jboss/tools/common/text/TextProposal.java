/******************************************************************************* 
 * Copyright (c) 2009 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.text;

import java.util.Comparator;

import org.eclipse.swt.graphics.Image;

/**
 * Text Proposal for Content Assist.
 * @author Alexey Kazakov
 */
public class TextProposal {

	public interface PostProcessing {
		public void process(TextProposal proposal, String value, int offset);
	}

	public static final int R_NONE = 0;
	public static final int R_JSP_JSF_EL_VARIABLE_ATTRIBUTE_VALUE = 810;
	public static final int R_JSP_ATTRIBUTE_VALUE = 830;
	public static final int R_XML_ATTRIBUTE_VALUE = 850;
	public static final int R_XML_ATTRIBUTE_NAME = 910;
	public static final int R_TAG_INSERTION = 500;
	public static final int R_XML_ATTRIBUTE_VALUE_TEMPLATE = 91;
	
	private static final long serialVersionUID = 3257007635692926512L;

	private Object source;
	private String label;
	private String contextInfo;
	private Image image;
	private boolean emptyImage = true;
	private String replacementString;
	private boolean emptyContextInfo = true;
	private int relevance = R_NONE;
	private int position = -1;
	private boolean autoActivationContentAssistantAfterApplication = false;

	private int start = -1;
	private int end = -1;

	PostProcessing postProcessing;

	/**
	 * 
	 * @return
	 */
    public int getRelevance() {
        return relevance;
    }

    /**
     * 
     * @param relevance
     */
    public void setRelevance(int relevance) {
        this.relevance = relevance;
    }

	/**
	 * @return
	 */
	public String getContextInfo() {
		return contextInfo;
	}

	/**
	 * @return 
	 */
	public Image getImage() {
		return image;
	}

	/**
	 * @return
	 */
	public String getLabel() {
		return label;
	}

	/**
	 * @return
	 */
	public String getReplacementString() {
		return replacementString;
	}

	/**
	 * @param string
	 */
	public void setContextInfo(String string) {
		contextInfo = string;
		if(contextInfo != null) {
			emptyContextInfo = false; 
		}
	}

	/**
	 * @param string
	 */
	public void setImage(Image img) {
		this.image = img;
		if(this.image != null) {
			emptyImage = false; 
		}
	}

	/**
	 * @param string
	 */
	public void setLabel(String string) {
		label = string;
	}

	/**
	 * @param string
	 */
	public void setReplacementString(String string) {
		replacementString = string;
	}

	/**
	 * @return
	 */
	public boolean hasContextInfo() {
		return !emptyContextInfo;
	}

	/**
	 * @return
	 */
	public boolean hasImage() {
		return !emptyImage;
	}

	/**
	 * @return
	 */
	public int getPosition() {
		if(position==-1 && getReplacementString()!=null) {
			return getReplacementString().length();
		}
		return position;
	}

	/**
	 * @param i
	 */
	public void setPosition(int i) {
		position = i;
	}

	/**
	 * 
	 * @param lowerCase
	 */
	public void changeCase(boolean lowerCase) {
		if(lowerCase) {
			if(label!=null) label = label.toLowerCase();
			if(replacementString!=null) replacementString = replacementString.toLowerCase();
		} else {
			if(label!=null) label = label.toUpperCase();
			if(replacementString!=null) replacementString = replacementString.toUpperCase();
		}
	}

	/**
	 * 
	 *
	 */
	public void removeAutocompleteRequiredAttributes() {
		int endAttr = replacementString.lastIndexOf('"');
		if(endAttr!=-1) {
			int startAttr = replacementString.substring(0, replacementString.indexOf('"')).lastIndexOf(' ');
			String newReplacementString = replacementString.substring(0, startAttr);
			if(endAttr+1<replacementString.length()) {
				newReplacementString = newReplacementString + replacementString.substring(endAttr + 1);
			}
			replacementString = newReplacementString;
		}
	}

	/**
	 * 
	 * @return
	 */
	public boolean isCloseTag() {
		return label != null && label.startsWith("/"); //$NON-NLS-1$
	}

	/**
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuffer buffer = new StringBuffer();
		buffer.append("label: "); //$NON-NLS-1$
		buffer.append(label);
		buffer.append("\ncontextInfo: "); //$NON-NLS-1$
		buffer.append(contextInfo);
		buffer.append("\nreplacementString: "); //$NON-NLS-1$
		buffer.append(replacementString);

		return buffer.toString();
	}

	/**
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(Object o) {
		return label.compareTo(((TextProposal)o).getLabel());
	}

    /**
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object obj) {
		if(obj == this) {
			return true;
		}
		if((!(obj instanceof TextProposal)) || (obj == null)) {
			return false;
		}
		TextProposal anotherProposal = (TextProposal)obj;
		boolean labelB = false;
		boolean contextInfoB = false;
		boolean replacementStringB = false;
		if(this.label!=null) {
		    labelB = this.label.equals(anotherProposal.getLabel());
		} else if(anotherProposal.getLabel()==null) {
		    labelB = true;
		}
		if(this.contextInfo!=null) {
		    contextInfoB = this.contextInfo.equals(anotherProposal.getContextInfo());
		} else if(anotherProposal.getContextInfo()==null) {
		    contextInfoB = true;
		}
		if(this.replacementString!=null) {
		    replacementStringB = this.replacementString.equals(anotherProposal.getReplacementString());
		} else if(anotherProposal.getReplacementString()==null) {
		    replacementStringB = true;
		}
		return labelB&&contextInfoB&&replacementStringB;
    }

    /**
     * 
     * @return
     */
	public boolean autoActivationContentAssistantAfterApplication() {
		return autoActivationContentAssistantAfterApplication;
	}

	/**
	 * 
	 */
	public void setAutoActivationContentAssistantAfterApplication(boolean autoActivationContentAssistantAfterApplication) {
		this.autoActivationContentAssistantAfterApplication = autoActivationContentAssistantAfterApplication;
	}

	public void setStart(int n) {
		start = n;
	}

	public void setEnd(int n) {
		end = n;
	}

	public int getStart() {
		return start;
	}

	public int getEnd() {
		return end;
	}

	public void setPostProcessing(PostProcessing postProcessing) {
		this.postProcessing = postProcessing;
	}

	public void postProcess(String value, int offset) {
		if(postProcessing != null) postProcessing.process(this, value, offset);
	}

    /**
	 * @return the source
	 */
	public Object getSource() {
		return source;
	}

	/**
	 * @param source the source to set
	 */
	public void setSource(Object source) {
		this.source = source;
	}

	public static final Comparator<TextProposal> KB_PROPOSAL_ORDER = new TextProposalComparator();

    private static class TextProposalComparator implements Comparator<TextProposal> {
		public int compare(TextProposal p1, TextProposal p2) {
			int n1=p1.replacementString.length(), n2=p2.replacementString.length();
			for (int i1=0, i2=0; i1<n1 && i2<n2; i1++, i2++) {
				char c1 = p1.replacementString.charAt(i1);
				char c2 = p2.replacementString.charAt(i2);
				if (c1 != c2) {
					c1 = Character.toUpperCase(c1);
					c2 = Character.toUpperCase(c2);
					if (c1 != c2) {
					   c1 = Character.toLowerCase(c1);
					   c2 = Character.toLowerCase(c2);
					   if (c1 != c2) {
					       return c1 - c2;
					   }
					}
				}
			}
			return n1 - n2;
		}
	}
}