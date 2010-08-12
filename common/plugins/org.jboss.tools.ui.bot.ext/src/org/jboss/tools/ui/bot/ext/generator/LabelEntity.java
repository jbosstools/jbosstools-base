package org.jboss.tools.ui.bot.ext.generator;

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Vector;

public class LabelEntity {

	
	private String name;
	private List<String> path;
	private Set<String> textFields;
	private Set<String> chbFields;
	public void setName(String name) {		
		this.name = name;
	}public String getName() {
		return name;
	}
	public List<String> getPath() {
		if (path==null) {
			path = new Vector<String>();
		}
		return path;
	}
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		for (String s : getPath()) {
			sb.append(s+"->");
		}
		sb.delete(sb.length()-2,sb.length());
		sb.append(" " + getName());
		return sb.toString();
	}
	@Override
	public boolean equals(Object obj) {
		if (obj.getClass().equals(this.getClass())) {
			return ((LabelEntity)obj).getName().equals(this.getName());
		}
		return super.equals(obj);
	}
	@Override
	public int hashCode() {
		return getName().hashCode();
	}
	public Set<String> getTextFields() {
		if (textFields==null) {
			textFields=new HashSet<String>();
		}
		return textFields;
	}
	public Set<String> getChbFields() {
		if (chbFields==null) {
			chbFields=new HashSet<String>();
		}
		return chbFields;
	}
	private static String commonFiltering(String name) {
		return name.replaceAll("\\=|\\?|,|\\[|\\]|\\'|\\:|\\.|\\(|\\)|-|/|\\*", "");
	}
	public String getDescriptionText() {
		StringBuilder sb = new StringBuilder();
		Iterator<String> iter = this.getPath().iterator();
		if (iter.hasNext()) {
			String item=iter.next();
			sb.append(item);
			while (iter.hasNext()) {
				sb.append("->");
				sb.append(iter.next());
			}
		}
		return sb.toString();
	}
	public  String getConstantName() {
		StringBuilder sb = new StringBuilder();
		Iterator<String> iter = this.getPath().iterator();
		if (iter.hasNext()) {
			String item=commonFiltering(iter.next());
			sb.append(item.replaceAll(" ", "_").toUpperCase());
			while (iter.hasNext()) {
				sb.append("__");
				sb.append(commonFiltering(iter.next()).replaceAll(" ", "_").toUpperCase());
			}
		} else {
			sb.append(commonFiltering(this.getName()).replaceAll(" ", "_").toUpperCase());
		}
		return sb.toString();
	}
	public static String getConstantName(String s) {
		return commonFiltering(s).replaceAll(" ", "_").toUpperCase();
	}
	public  String getClassName() {
		StringBuilder sb = new StringBuilder();
		Iterator<String> iter = this.getPath().iterator();
		if (iter.hasNext()) {
			String item=commonFiltering(iter.next());
			sb.append(item.replaceAll(" ", ""));
			while (iter.hasNext()) {
				//sb.append("X");
				sb.append(commonFiltering(iter.next()).replaceAll(" ", ""));
			}
		} else {
			sb.append(commonFiltering(this.getName()).replaceAll(" ", "").toUpperCase());
		}
		return sb.toString();
	}
}
