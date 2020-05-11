def manmaker(context):
  
  MANIFEST = """
  apiVersion: v1
  kind: Pod
  metadata:
    name: rstudio-host
  spec:
    containers:
    - name: rstudio-host
      image: gcr.io/ml-learning-199501/github.com/jasonmhoule/irpdr:latest
      imagePullPolicy: Always
      env:
      - name: PASSWORD
        value: {password}
      - name: USER
        value: {user}
      - name: ROOT
        value: true
      stdin: true
      tty: true
      ports:
      - containerPort: 8787
        hostPort: 8787
      restartPolicy: Always
  """.format(**context.properties)
  
  return MANIFEST
