package org.jboss.tools.ui.bot.ext.generator;

import java.io.File;
import java.io.PrintWriter;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

public class ActionItemWriter {

	private static final String outputDir = "/data/svn/jbosstools/jst/tests/org.jboss.tools.ui.bot.ext/src";
	private static final String packageName = "org.jboss.tools.ui.bot.ext.gen";
	private static final String packageDir = packageName.replaceAll("\\.", "/");
	private static final String interfaceName = "IActionItem";
	private static final String className = "ActionItem";
	private static final String appName="JBDS 3.0.0-#50-GA";
	private final Map<String, List<LabelEntity>> entityMap = new LinkedHashMap<String, List<LabelEntity>>();

	public Map<String, List<LabelEntity>> getEntityMap() {
		return entityMap;
	}

	private void genConstants(LabelEntity entity, PrintWriter pw,
			String prefix) {

		for (String constant : entity.getTextFields()) {
			addJavaDoc(pw, "text field labeled '" + constant + "'", prefix);
			pw.println(prefix + "public static final String TEXT_"
					+ LabelEntity.getConstantName(constant)
					+ " = \"" + constant + "\";");

		}
		for (String constant : entity.getChbFields()) {
			addJavaDoc(pw, "checkbox field labeled '" + constant + "'", prefix);
			pw.println(prefix + "public static final String CHB_"
					+ LabelEntity.getConstantName(constant)
					+ " = \"" + constant + "\";");
		}

	}
	private void writePrintMethod(PrintWriter pw, String prefix) {
		addJavaDoc(pw, "gets string representation of item", prefix);
		pw.println(prefix+"public static String getItemString(" + interfaceName + " item) {");
		pw.println(prefix);
		pw.println(prefix+"\tStringBuilder sb = new StringBuilder();");
		pw.println(prefix+"\tfor (String s :item.getGroupPath()) {");
		pw.println(prefix+"\t\tsb.append(s+\"->\");");
		pw.println(prefix+"\t}");
		pw.println(prefix+"\tsb.append(item.getName());");
		pw.println(prefix+"\treturn sb.toString();");
		pw.println(prefix+"}");
	}
	/**
	 * generates common IActionItem interface file, for each entity list in entityMap
	 * is also generated interface file derived from IActionItem
	 * @throws Exception
	 */
	public void generateInterfaces() throws Exception {
		PrintWriter pw = createFile(interfaceName);
		addHeader(pw);
		pw.println("import java.util.List;");
		pw.println();
		pw.println("public interface " + interfaceName + " {");
		pw.println();
		addJavaDoc(pw, "gets label name", "\t");
		pw.println("\tpublic String getName();");
		addJavaDoc(pw, "gets path (in tree,list) to reach leaf returned by 'getName()'", "\t");
		pw.println("\tpublic List<String> getGroupPath();");
		pw.println();
		pw.println("}");
		pw.println();
		pw.close();
		System.out.println("DONE");
		generateSubIntefaces();
	}

	private void generateSubIntefaces() throws Exception {
		
		for (String key : entityMap.keySet()) {
			PrintWriter pw = createFile("I"+key);
			addHeader(pw);
			pw.println("import "+packageName+"."+interfaceName+";");
			pw.println();
			pw.println("public interface I" + key + " extends "+interfaceName+" {");
			pw.println("}");
			pw.close();
			System.out.println("DONE");
			pw.println();
		}

	}
	
	/**
	 * generates class files of entities
	 * @throws Exception
	 */
	public void generateClasses() throws Exception {
		PrintWriter pw = createFile(className);
		addHeader(pw);
		pw.println("import java.util.List;");
		pw.println("import java.util.Vector;");
		pw.println();
		pw.println("public class " + className + " {");
		pw.println();
		writePrintMethod(pw, "");
		for (String key : entityMap.keySet()) {
			// first remove possible duplicates
			Set<LabelEntity> set =new HashSet<LabelEntity>();
			for (LabelEntity ent : entityMap.get(key)) {
				set.add(ent);
			}
			List<LabelEntity> list = new Vector<LabelEntity>();
			for (LabelEntity le : set) {
				list.add(le);
			}
			genLEClass(pw, key, list, "\t");
			pw.println();
		}
		pw.println("}");
		pw.close();
		System.out.println("DONE");
	}

	private static void genIFaceImplementation(LabelEntity entity, PrintWriter pw,
			String prefix, String ifaceName) {

		addJavaDoc(pw, "represents item : "
			+ entity.getDescriptionText(), prefix);
		pw.println(prefix + "public static final " + ifaceName
				+ " LABEL = new " + ifaceName + "() {");
		pw.println(prefix + "\tpublic String getName() { return \""
				+ entity.getName() + "\";}");
		pw.println(prefix + "\tpublic List<String> getGroupPath() {");
		pw.println(prefix + "\t\tList<String> l = new Vector<String>();");
		for (int i = 0; i < entity.getPath().size() - 1; i++) {
			pw.println(prefix + "\t\tl.add(\"" + entity.getPath().get(i)
					+ "\");");
		}
		pw.println(prefix + "\t\treturn l;");
		pw.println(prefix + "\t}");
		pw.println(prefix + "};");
	}

	private void genLEClass(PrintWriter pw, String SubClassName,
			List<LabelEntity> items, String prefix) {		
		pw.println(prefix + "public static class " + SubClassName + " {");
		prefix=prefix+"\t";
		for (LabelEntity le : items) {
			pw.println(prefix + "public static class "
					+ le.getClassName() + " {");
			genIFaceImplementation(le, pw, prefix + "\t","I"+SubClassName);
			genConstants(le, pw, prefix+"\t");
			pw.println(prefix + "\t}");
		}
		pw.println();
		pw.println(prefix + "}");
	}

	private static void addJavaDoc(PrintWriter pw,String content, String prefix) {
		pw.println(prefix+"/**");
		pw.println(prefix+"* "+content );
		pw.println(prefix+"*/");
	}
	private void addHeader(PrintWriter pw) {		
		addJavaDoc(pw, "Generated "+ new Date()+ " "+appName, "");
		pw.println("package " + packageName + ";");
		pw.println();
	}

	private PrintWriter createFile(String className) throws Exception {
		String file = outputDir + "/" + packageDir + "/" + className + ".java";
		System.out.println("Generating file " + file);
		new File(outputDir + "/" + packageDir).mkdirs();
		return new PrintWriter(new File(file));
	}

	public static void main(String[] args) {
		try {
			new ActionItemWriter().generateInterfaces();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
